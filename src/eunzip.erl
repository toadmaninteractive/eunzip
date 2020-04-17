%% @author: Maxim Pushkar
%% @date: 06.04.2020

%% Supports STORE and DEFLATE compression methods
%% Supports Zip64 specification

-module(eunzip).

%% Include files
-include_lib("stdlib/include/assert.hrl").
-include("eunzip.hrl").

%% API
-export([
    open/1,
    close/1,
    verify/2,
    decompress/3,
    stream/4
]).

% Stream state
-record(stream_state, {
    zip_handle :: file:fd(),
    offset :: non_neg_integer(),
    end_offset :: non_neg_integer(),
    compression_method :: ?M_STORE | ?M_DEFLATE | non_neg_integer(),
    z_stream :: zlib:zstream() | undefined,
    crc :: non_neg_integer(),
    acc_crc :: non_neg_integer() | 'undefined',
    stream_fun :: function() | 'undefined',
    stream_acc :: any()
}).

% Decompress state
-record(decompress_state, {
    file_name :: file:filename_all(),
    fd :: file:fd()
}).

%% API

%% Open Zip file
-spec open(FileName :: file:filename_all()) ->
    {'ok', eunzip_types:unzip_state()} | {'error', Reason :: atom()}.

open(FileName) ->
    FileSize = filelib:file_size(FileName),
    case file:open(FileName, [read, binary, raw]) of
        {ok, ZipHandle} ->
            case eunzip_central_dir:eocd(ZipHandle, FileSize) of
                {ok, Eocd} ->
                    case eunzip_central_dir:entries(ZipHandle, FileSize, Eocd) of
                        {ok, CentralDir} ->
                            {ok, #unzip_state{zip_handle = ZipHandle, central_dir = CentralDir, file_size = FileSize}};
                        {error, Reason} ->
                            file:close(ZipHandle),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    file:close(ZipHandle),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Close Zip file
-spec close(State :: eunzip_types:unzip_state()) ->
    'ok' | {'error', Reason :: atom()}.

close(#unzip_state{zip_handle = ZipHandle}) ->
    file:close(ZipHandle).

-spec verify(UnzipState, FileName) -> Result when
    UnzipState :: eunzip_types:unzip_state(),
    FileName :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

verify(#unzip_state{zip_handle = ZipHandle, central_dir = CentralDir}, FileName) ->
    case maps:get(FileName, CentralDir, undefined) of
        #cd_entry{compression_method = Method, compressed_size = CompressedSize, local_header_offset = LocalHeaderOffset, crc = Crc} ->
            StartOffset = file_start_offset(ZipHandle, LocalHeaderOffset),
            case stream_file(ZipHandle, Method, StartOffset, CompressedSize, Crc, undefined, undefined) of
                {ok, _} -> ok;
                {error, Reason, _} -> {error, Reason}
            end;
        undefined ->
            {error, file_not_found}
    end.

-spec decompress(UnzipState, CdFileName, TargetFileName) -> Result when
    UnzipState :: eunzip_types:unzip_state(),
    CdFileName :: binary(),
    TargetFileName :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

decompress(#unzip_state{zip_handle = ZipHandle, central_dir = CentralDir}, CdFileName, TargetFileName) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        #cd_entry{compression_method = Method, compressed_size = CompressedSize, local_header_offset = LocalHeaderOffset, crc = Crc} ->
            FileExists = filelib:is_file(TargetFileName) orelse filelib:is_dir(TargetFileName),
            case FileExists of
                true -> {error, target_file_exists};
                false ->
                    StartOffset = file_start_offset(ZipHandle, LocalHeaderOffset),
                    case stream_file(ZipHandle, Method, StartOffset, CompressedSize, Crc, fun decompress_stream/2, #decompress_state{file_name = TargetFileName}) of
                        {ok, _} -> ok;
                        {error, Reason, _} -> {error, Reason}
                    end
                end;
        undefined ->
            {error, file_not_found}
    end.

-spec stream(UnzipState, CdFileName, StreamFun, StreamAcc) -> Result when
    UnzipState :: eunzip_types:unzip_state(),
    CdFileName :: binary(),
    StreamFun :: fun((StreamFunArg, T) -> T),
    StreamFunArg :: 'bof' | {'data', binary()} | 'eof' | {'error', atom()},
    StreamAcc :: any(),
    Result :: {'ok', T :: any()} | {'error', Reason :: atom()}.

stream(#unzip_state{zip_handle = ZipHandle, central_dir = CentralDir}, CdFileName, StreamFun, StreamAcc) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        #cd_entry{compression_method = Method, compressed_size = CompressedSize, local_header_offset = LocalHeaderOffset, crc = Crc} ->
            StartOffset = file_start_offset(ZipHandle, LocalHeaderOffset),
            case stream_file(ZipHandle, Method, StartOffset, CompressedSize, Crc, StreamFun, StreamAcc) of
                {ok, StreamAcc1} -> {ok, StreamAcc1};
                {error, Reason, _} -> {error, Reason}
            end;
        undefined ->
            {error, file_not_found}
    end.

%% Internal functions
safe_call(Fun, Args, Default) ->
    ArgCount = length(Args),
    case is_function(Fun, ArgCount) of
        true -> erlang:apply(Fun, Args);
        false -> Default
    end.

file_start_offset(ZipHandle, LocalHeaderOffset) ->
    case file:pread(ZipHandle, LocalHeaderOffset, ?local_header_size) of
        {ok, LocalHeader} ->
            <<
                16#04034b50:32/little,
                _:32/little,
                _CompressionMethod:16/little,
                _:128/little,
                FileNameLength:16/little,
                ExtraFieldLength:16/little
            >> = LocalHeader,
            LocalHeaderOffset + ?local_header_size + FileNameLength + ExtraFieldLength;
        {error, Reason} ->
            {error, Reason}
    end.

stream_file(ZipHandle, CompressionMethod, Offset, Size, Crc, StreamFun, StreamAcc) ->
    State = #stream_state{
        zip_handle = ZipHandle,
        offset = Offset,
        end_offset = Offset + Size,
        compression_method = CompressionMethod,
        crc = Crc,
        acc_crc = erlang:crc32(<<>>)
    },
    ZStream = case CompressionMethod of
        ?M_STORE -> undefined;
        ?M_DEFLATE ->
            Z = zlib:open(),
            ok = zlib:inflateInit(Z, -15),
            ok = zlib:setBufSize(Z, ?z_buffer_size),
            Z
    end,
    StreamAcc1 = safe_call(StreamFun, [bof, StreamAcc], StreamAcc),
    Result = case stream_file_fun(State#stream_state{z_stream = ZStream, stream_fun = StreamFun, stream_acc = StreamAcc1}) of
        {ok, StreamAcc2} -> {ok, StreamAcc2};
        {error, Reason, _StreamAcc2} -> {error, Reason}
    end,
    case CompressionMethod of
        ?M_STORE -> undefined;
        ?M_DEFLATE ->
            ok = zlib:inflateEnd(ZStream),
            zlib:close(ZStream)
    end,
    Result.

stream_file_fun(#stream_state{
    offset = Offset,
    end_offset = EndOffset,
    crc = Crc,
    acc_crc = Crc,
    stream_fun = StreamFun,
    stream_acc = StreamAcc
}) when Offset >= EndOffset ->
    StreamAcc1 = safe_call(StreamFun, [eof, StreamAcc], StreamAcc),
    {ok, StreamAcc1};

stream_file_fun(#stream_state{
    offset = Offset,
    end_offset = EndOffset,
    stream_fun = StreamFun,
    stream_acc = StreamAcc
}) when Offset >= EndOffset ->
    Reason = crc32_mismatch,
    StreamAcc1 = safe_call(StreamFun, [{error, Reason}, StreamAcc], StreamAcc),
    {error, Reason, StreamAcc1};

stream_file_fun(#stream_state{
    zip_handle = ZipHandle,
    compression_method = Method,
    z_stream = Z,
    offset = Offset,
    end_offset = EndOffset,
    acc_crc = AccCrc,
    stream_fun = StreamFun,
    stream_acc = StreamAcc
} = State) ->
    NextOffset = min(Offset + ?file_chunk_size, EndOffset),
    case file:pread(ZipHandle, Offset, NextOffset - Offset) of
        {ok, RawData} ->
            DecompressedData = case Method of
                ?M_STORE -> RawData;
                ?M_DEFLATE -> collect(Z, RawData)
            end,
            AccCrc1 = erlang:crc32(AccCrc, DecompressedData),
            StreamAcc1 = safe_call(StreamFun, [{data, DecompressedData}, StreamAcc], StreamAcc),
            stream_file_fun(State#stream_state{offset = NextOffset, acc_crc = AccCrc1, stream_acc = StreamAcc1});
        {error, Reason} ->
            StreamAcc1 = safe_call(StreamFun, [{error, Reason}, StreamAcc], StreamAcc),
            {error, Reason, StreamAcc1}
    end.

collect(Z, CompressedData) ->
    collect(Z, <<>>, zlib:inflateChunk(Z, CompressedData)).

collect(Z, Acc, {more, Decompressed}) ->
    collect(Z, <<Acc/binary, (iolist_to_binary(Decompressed))/binary>>, zlib:inflateChunk(Z));
collect(_Z, Acc, Decompressed) ->
    <<Acc/binary, (iolist_to_binary(Decompressed))/binary>>.

decompress_stream(bof, #decompress_state{file_name = FileName} = State) ->
    ok = filelib:ensure_dir(FileName),
    {ok, FD} = file:open(FileName, [write, binary, raw]),
    State#decompress_state{fd = FD};

decompress_stream({error, _Reason}, #decompress_state{fd = FD} = State) ->
    ok = file:close(FD),
    State#decompress_state{fd = undefined};

decompress_stream({data, Data}, #decompress_state{fd = FD} = State) ->
    ok = file:write(FD, Data),
    State;

decompress_stream(eof, #decompress_state{fd = FD} = State) ->
    ok = file:close(FD),
    State#decompress_state{fd = undefined}.

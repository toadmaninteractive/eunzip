%% @author: Maxim Pushkar
%% @date: 06.04.2020

%% Supports STORE and DEFLATE compression methods
%% Supports Zip64 specification

-module(eunzip).

%% Include files
-include("eunzip.hrl").

%% API
-export([
    open/1,
    close/1,
    entries/1,
    entry/2,
    is_file/2,
    is_dir/2,
    verify/2,
    decompress/3,
    stream_init/2,
    stream_read_chunk/2,
    stream_end/1
]).

% Types
-opaque unzip_state() :: #unzip_state{}.
-export_type([unzip_state/0]).

-opaque cd_info() :: #cd_info{}.
-export_type([cd_info/0]).

-type cd_entry() :: #cd_entry{}.
-export_type([cd_entry/0]).

-opaque file_buffer() :: #file_buffer{}.
-export_type([file_buffer/0]).

-opaque direction() :: 'backward' | 'forward'.
-export_type([direction/0]).

-opaque stream_state() :: #stream_state{}.
-export_type([stream_state/0]).

%% API

%% Open Zip file
-spec open(FileName :: file:filename_all()) ->
    {'ok', unzip_state()} | {'error', Reason :: atom()}.

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
-spec close(State :: unzip_state()) ->
    'ok' | {'error', Reason :: atom()}.

close(#unzip_state{zip_handle = ZipHandle}) ->
    file:close(ZipHandle).

% Get all file and directory entries from Zip archive
-spec entries(UnzipState) -> Result when
    UnzipState :: unzip_state(),
    Result :: {'ok', Entries :: [cd_entry()]} | {'error', Reason :: atom()}.

entries(#unzip_state{central_dir = CentralDir}) ->
    {ok, maps:values(CentralDir)};

entries(_) ->
    {error, invalid_unzip_state}.

% Get file or directory entry from Zip archive
-spec entry(UnzipState, CdFileName) -> Result when
    UnzipState :: unzip_state(),
    CdFileName :: file:filename_all(),
    Result :: {'ok', cd_entry()} | {'error', Reason :: atom()}.

entry(UnzipState, CdFileName) when is_list(CdFileName) ->
    entry(UnzipState, iolist_to_binary(CdFileName));

entry(#unzip_state{central_dir = CentralDir}, CdFileName) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        undefined -> {error, not_exists};
        #cd_entry{} = Entry -> {ok, Entry}
    end;

entry(_, _) ->
    {error, invalid_unzip_state}.

% Check is supplied file name is a regular file
-spec is_file(UnzipState, CdFileName) -> Result when
    UnzipState :: unzip_state(),
    CdFileName :: file:filename_all(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_file(UnzipState, CdFileName) when is_list(CdFileName) ->
    is_file(UnzipState, iolist_to_binary(CdFileName));

is_file(#unzip_state{central_dir = CentralDir}, CdFileName) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        undefined -> {error, not_exists};
        #cd_entry{is_regular_file = IsRegularFile} -> {ok, IsRegularFile}
    end;

is_file(_, _) ->
    {error, invalid_unzip_state}.

% Check is supplied file name is a directory
-spec is_dir(UnzipState, CdFileName) -> Result when
    UnzipState :: unzip_state(),
    CdFileName :: file:filename_all(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_dir(UnzipState, CdFileName) when is_list(CdFileName) ->
    is_dir(UnzipState, iolist_to_binary(CdFileName));

is_dir(#unzip_state{central_dir = CentralDir}, CdFileName) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        undefined -> {error, not_exists};
        #cd_entry{is_regular_file = IsRegularFile} -> {ok, not IsRegularFile}
    end;

is_dir(_, _) ->
    {error, invalid_unzip_state}.

% Verify archived file checksum
-spec verify(UnzipState, CdFileName) -> Result when
    UnzipState :: unzip_state(),
    CdFileName :: file:filename_all(),
    Result :: 'ok' | {'error', Reason :: atom()}.

verify(UnzipState, CdFileName) when is_list(CdFileName) ->
    verify(UnzipState, iolist_to_binary(CdFileName));

verify(#unzip_state{central_dir = CentralDir} = UnzipState, CdFileName) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        #cd_entry{is_regular_file = false} ->
            {error, is_dir};

        #cd_entry{compression_method = Method} when not (Method =:= ?M_STORE orelse Method =:= ?M_DEFLATE) ->
            {error, compression_method_not_supported};

        #cd_entry{} ->
            case stream_init(UnzipState, CdFileName) of
                {ok, StreamState} -> verify_stream(StreamState);
                {error, Reason} -> {error, Reason}
            end;

        undefined ->
            {error, file_not_found}
    end;

verify(_, _) ->
    {error, invalid_unzip_state}.

% Decompress archived file into given path
-spec decompress(UnzipState, CdFileName, TargetFileName) -> Result when
    UnzipState :: unzip_state(),
    CdFileName :: file:filename_all(),
    TargetFileName :: file:filename_all(),
    Result :: 'ok' | {'error', Reason :: atom()}.

decompress(UnzipState, CdFileName, TargetFileName) when is_list(CdFileName) ->
    decompress(UnzipState, iolist_to_binary(CdFileName), TargetFileName);

decompress(UnzipState, CdFileName, TargetFileName) when is_list(TargetFileName) ->
    decompress(UnzipState, CdFileName, iolist_to_binary(TargetFileName));

decompress(#unzip_state{central_dir = CentralDir} = UnzipState, CdFileName, TargetFileName) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        #cd_entry{is_regular_file = false} ->
            {error, is_dir};

        #cd_entry{compression_method = Method} when not (Method =:= ?M_STORE orelse Method =:= ?M_DEFLATE) ->
            {error, compression_method_not_supported};

        #cd_entry{} ->
            case filelib:ensure_dir(TargetFileName) of
                ok ->
                    case file:open(TargetFileName, [write, binary, raw]) of
                        {ok, Fd} ->
                            case stream_init(UnzipState, CdFileName) of
                                {ok, StreamState} ->
                                    case decompress_stream(StreamState, Fd) of
                                        ok ->
                                            file:close(Fd);
                                        {error, _Reason} = Error ->
                                            file:close(Fd),
                                            file:delete(TargetFileName),
                                            Error
                                    end;
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;

        undefined ->
            {error, file_not_found}
    end;

decompress(_, _, _) ->
    {error, invalid_unzip_state}.

% Initialize archived file stream, call stream_read_chunk/1 to get actual decompressed data chunks
-spec stream_init(UnzipState, CdFileName) -> Result when
    UnzipState :: unzip_state(),
    CdFileName :: file:filename_all(),
    Result :: {'ok', stream_state()} | {'error', Reason :: atom()}.

stream_init(UnzipState, CdFileName) when is_list(CdFileName) ->
    stream_init(UnzipState, iolist_to_binary(CdFileName));

stream_init(#unzip_state{zip_handle = ZipHandle, central_dir = CentralDir}, CdFileName) ->
    case maps:get(CdFileName, CentralDir, undefined) of
        #cd_entry{is_regular_file = false} ->
            {error, is_dir};

        #cd_entry{compression_method = Method} when not (Method =:= ?M_STORE orelse Method =:= ?M_DEFLATE) ->
            {error, compression_method_not_supported};

        #cd_entry{compression_method = Method, compressed_size = CompressedSize, local_header_offset = LocalHeaderOffset, crc = Crc} ->
            case eunzip_util:file_start_offset(ZipHandle, LocalHeaderOffset) of
                {ok, StartOffset} ->
                    StreamState = #stream_state{
                        filename = CdFileName,
                        zip_handle = ZipHandle,
                        offset = StartOffset,
                        end_offset = StartOffset + CompressedSize,
                        compression_method = Method,
                        crc = Crc,
                        acc_crc = erlang:crc32(<<>>)
                    },
                    case eunzip_util:zlib_init(Method) of
                        {ok, ZStream} -> {ok, StreamState#stream_state{z_stream = ZStream}};
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;

        undefined ->
            {error, file_not_found}
    end;

stream_init(_, _) ->
    {error, invalid_unzip_state}.

% Reads chun of compressed file data and decompress it
stream_read_chunk(_, #stream_state{chunks_read = 0, offset = Offset, end_offset = EndOffset} = State) when Offset >= EndOffset ->
    {ok, <<>>, State#stream_state{chunks_read = 1}};

stream_read_chunk(_, #stream_state{offset = Offset, end_offset = EndOffset} = State) when Offset >= EndOffset ->
    {eof, State};

stream_read_chunk(ChunkSize, #stream_state{offset = Offset, end_offset = EndOffset} = State) ->
    #stream_state{
        zip_handle = ZipHandle,
        compression_method = Method,
        z_stream = Z,
        crc = Crc,
        acc_crc = AccCrc,
        chunks_read = ChunksRead
    } = State,
    NextOffset = min(Offset + ChunkSize, EndOffset),
    case file:pread(ZipHandle, Offset, NextOffset - Offset) of
        {ok, RawData} ->
            DecompressedData = case Method of
                ?M_STORE -> RawData;
                ?M_DEFLATE -> eunzip_util:zlib_collect(Z, RawData)
            end,
            AccCrc1 = erlang:crc32(AccCrc, DecompressedData),
            case NextOffset of
                EndOffset when AccCrc1 =:= Crc ->
                    {ok, DecompressedData, #stream_state{offset = NextOffset, acc_crc = AccCrc1, chunks_read = ChunksRead + 1}};
                EndOffset ->
                    {error, crc_mismatch};
                _ ->
                    {more, DecompressedData, #stream_state{offset = NextOffset, acc_crc = AccCrc1, chunks_read = ChunksRead + 1}}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

stream_read_chunk(_, _) ->
    {error, invalid_stream_state}.

% Finalizes streaming of an archived file. Performs internal structure cleanup
stream_end(#stream_state{z_stream = undefined}) ->
    ok;

stream_end(#stream_state{z_stream = Z}) ->
    eunzip_util:zlib_end(Z);

stream_end(_) ->
    {error, invalid_stream_state}.

%% Internal functions
verify_stream(StreamState) ->
    case stream_read_chunk(?file_chunk_size, StreamState) of
        {ok, _Data, StreamState1} ->
            stream_end(StreamState1);
        {more, _Data, StreamState1} ->
            verify_stream(StreamState1);
        {error, Reason} ->
            stream_end(StreamState),
            {error, Reason}
    end.

decompress_stream(StreamState, Fd) ->
    case stream_read_chunk(?file_chunk_size, StreamState) of
        {ok, Data, StreamState1} ->
            file:write(Fd, Data),
            stream_end(StreamState1);
        {more, Data, StreamState1} ->
            file:write(Fd, Data),
            decompress_stream(StreamState1, Fd);
        {error, Reason} ->
            stream_end(StreamState),
            {error, Reason}
    end.

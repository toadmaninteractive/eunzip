%% @author: Maxim Pushkar
%% @date: 18.04.2020

-module(eunzip_util).

%% Include files
-include("eunzip.hrl").

%% API
-export([
    file_start_offset/2,
    zlib_init/1,
    zlib_end/1,
    zlib_collect/2
]).

%% API
-spec file_start_offset(ZipHandle, LocalHeaderOffset) -> Result when
    ZipHandle :: file:fd(),
    LocalHeaderOffset :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

file_start_offset(ZipHandle, LocalHeaderOffset) ->
    case file:pread(ZipHandle, LocalHeaderOffset, ?local_header_size) of
        {ok, <<16#04034b50:32/little, _:32/little, _:16/little, _:128/little, FileNameLength:16/little, ExtraFieldLength:16/little>>} ->
            {ok, LocalHeaderOffset + ?local_header_size + FileNameLength + ExtraFieldLength};
        {ok, _} ->
            {error, invalid_file_header};
        {error, Reason} ->
            {error, Reason}
    end.

-spec zlib_init(CompressionMethod) -> Result when
    CompressionMethod :: ?M_DEFLATE | non_neg_integer(),
    Result :: {'ok', zlib:zstream()} | {'error', 'zlib_init_failed'}.

zlib_init(?M_DEFLATE) ->
    try
        Z = zlib:open(),
        ok = zlib:inflateInit(Z, -15),
        {ok, Z}
    catch
        _:_ -> {error, zlib_init_failed}
    end;

zlib_init(_) ->
    {ok, undefined}.

-spec zlib_end(Z) -> Result when
    Z :: zlib:zstream(),
    Result :: 'ok' | {'error', 'zlib_end_failed'}.

zlib_end(Z) ->
    try
        ok = zlib:inflateEnd(Z),
        ok = zlib:close(Z)
    catch
        _:_ -> {error, zlib_end_failed}
    end.

-spec zlib_collect(Z, CompressedData) -> Result when
    Z :: zlib:zstream(),
    CompressedData :: binary(),
    Result :: binary().

zlib_collect(Z, CompressedData) ->
    zlib_collect(Z, <<>>, zlib:inflateChunk(Z, CompressedData)).

%% Internal functions
zlib_collect(Z, Acc, {more, Decompressed}) ->
    zlib_collect(Z, <<Acc/binary, (iolist_to_binary(Decompressed))/binary>>, zlib:inflateChunk(Z));
zlib_collect(_Z, Acc, Decompressed) ->
    <<Acc/binary, (iolist_to_binary(Decompressed))/binary>>.

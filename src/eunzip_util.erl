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
    zlib_collect/2,
    detect_compression_format/1
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
        ok = zlib:setBufSize(Z, 512 * 1024),
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

-spec comp_format_binary_to_atom(CompFormatBin) -> Result when
  CompFormatBin :: binary(),
  Result :: atom().

comp_format_binary_to_atom(CompFormatBin) ->
  case CompFormatBin of 
    <<8,0>> -> deflate;
    <<9,0>> -> deflate64;
    <<0,0>> -> stored;
    _ -> unknown_format
  end.

-spec open_zip(Filename) -> Result when
  Filename :: string(),
  Result :: {'ok', binary()} | {'error', atom()}.

open_zip(Filename) ->
  case file:open(Filename, [read, binary]) of 
    {ok, FileDescriptor} ->
      case file:pread(FileDescriptor, 0, 10) of 
        {ok, Data} -> 
          ok = file:close(FileDescriptor),
          {ok, Data};
        {error, Reason} -> 
          ok = file:close(FileDescriptor),
          {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.

%% Detect compression format
-spec detect_compression_format(Filename) -> Result when
  Filename :: string(),
  Result :: {'ok', atom()} | {'error', atom()}.

detect_compression_format(Filename) ->
  case open_zip(Filename) of 
    {ok, Data} -> 
      <<80, 75, 3, 4, _D:4/binary, CompressionFormat:2/binary, _MoreData/binary>> = Data,
      {ok, comp_format_binary_to_atom(CompressionFormat)};
    {error, Reason} -> {error, Reason}
  end.

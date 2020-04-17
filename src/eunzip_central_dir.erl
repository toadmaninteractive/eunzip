%% @author: Maxim Pushkar
%% @date: 14.04.2020

-module(eunzip_central_dir).

%% Include files
-include("eunzip.hrl").

%% API
-export([
    eocd/2,
    entries/3
]).

%% API
-spec eocd(ZipHandle, FileSize) -> Result when
    ZipHandle :: file:fd(),
    FileSize :: non_neg_integer(),
    Result :: {'ok', Eocd :: eunzip_types:cd_info()} | {'error', Reason :: atom()}.

eocd(ZipHandle, FileSize) ->
    case eunzip_buffer:new(ZipHandle, FileSize, ?zip_chunk_size) of
        {ok, FileBuffer} ->
            case find_eocd(FileBuffer, 0) of
                {ok, Eocd, FileBuffer1} ->
                    case find_zip64_eocd(FileBuffer1) of
                        {ok, Zip64Eocd} -> {ok, Zip64Eocd};
                        _ -> {ok, Eocd}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec entries(ZipHandle, FileSize, CdInfo) -> Result when
    ZipHandle :: file:fd(),
    FileSize :: non_neg_integer(),
    CdInfo :: eunzip_types:cd_info(),
    Result :: {'ok', Eocd :: eunzip_types:cd_info()} | {'error', Reason :: atom()}.

entries(ZipHandle, FileSize, #cd_info{cd_offset = CdOffset, cd_size = CdSize}) ->
    case eunzip_buffer:new(ZipHandle, FileSize, ?zip_chunk_size, CdOffset + CdSize, CdOffset, forward) of
        {ok, FileBuffer} -> parse_cd(FileBuffer, #{entries => #{}, range_tree => eunzip_range:new()});
        {error, Reason} -> {error, Reason}
    end.

%% Internal functions

% Spec has variable length comment at the end of zip after EOCD, so EOCD can anywhere in the zip file.
% To avoid exhaustive search, we limit search space to last 5 MB.
% If we don't find EOCD within that we assume it's an invalid zip

find_eocd(_FileBuffer, Consumed) when Consumed > ?eocd_search_limit ->
    {error, missing_eocd_record};

find_eocd(FileBuffer, Consumed) ->
    case eunzip_buffer:next_chunk(FileBuffer, ?eocd_header_size) of
        {ok, Chunk, FileBuffer1} ->
            case Chunk of
                <<16#06054b50:32/little, _:48/little, TotalEntries:16/little, CdSize:32/little, CdOffset:32/little, Consumed:16/little>> ->
                    {ok, FileBuffer2} = eunzip_buffer:move_backward_by(FileBuffer1, ?eocd_header_size),
                    {ok, #cd_info{total_entries = TotalEntries, cd_size = CdSize, cd_offset = CdOffset}, FileBuffer2};

                Chunk when byte_size(Chunk) < ?eocd_header_size ->
                    {error, missing_eocd_record};

                _ ->
                    {ok, FileBuffer2} = eunzip_buffer:move_backward_by(FileBuffer1, 1),
                    find_eocd(FileBuffer2, Consumed + 1)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

is_zip64(<<16#07064b50:32/little, _:128/little>>) -> true;
is_zip64(_) -> false.

find_zip64_eocd(#file_buffer{file = ZipHandle} = FileBuffer) ->
    case eunzip_buffer:next_chunk(FileBuffer, ?zip64_eocd_locator_size) of
        {ok, Chunk, _FileBuffer} ->
            case is_zip64(Chunk) of
                true ->
                    <<16#07064b50:32/little, _:32/little, EocdOffset:64/little, _:32/little>> = Chunk,
                    {ok, Buffer} = file:pread(ZipHandle, EocdOffset, ?zip64_eocd_size),
                    <<16#06064b50:32/little, _:64, _:16, _:16, _:32, _:32, _:64, TotalEntries:64/little, CdSize:64/little, CdOffset:64/little>> = Buffer,
                    {ok, #cd_info{total_entries = TotalEntries, cd_size = CdSize, cd_offset = CdOffset}};
                false ->
                    {error, not_a_zip64}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_cd(#file_buffer{buffer_position = Pos, limit = Limit}, #{entries := Entries}) when Pos >= Limit ->
    {ok, Entries};

parse_cd(FileBuffer, Acc) ->
    case eunzip_buffer:next_chunk(FileBuffer, 46) of
        {ok, Chunk, FileBuffer1} ->
            <<
                16#02014b50:32/little,
                _:32/little,
                Flag:16/little,
                CompressionMethod:16/little,
                MTime:16/little,
                MDate:16/little,
                Crc:32/little,
                CompressedSize:32/little,
                UncompressedSize:32/little,
                FileNameLength:16/little,
                ExtraFieldLength:16/little,
                CommentLength:16/little,
                _:64/little,
                LocalHeaderOffset:32/little
            >> = Chunk,
            {ok, FileBuffer2} = eunzip_buffer:move_forward_by(FileBuffer1, 46),
            {ok, FileName, FileBuffer3} = eunzip_buffer:next_chunk(FileBuffer2, FileNameLength),
            {ok, FileBuffer4} = eunzip_buffer:move_forward_by(FileBuffer3, FileNameLength),
            {ok, ExtraFields, FileBuffer5} = eunzip_buffer:next_chunk(FileBuffer4, ExtraFieldLength),
            {ok, FileBuffer6} = eunzip_buffer:move_forward_by(FileBuffer5, ExtraFieldLength),
            {ok, _FileComment, FileBuffer7} = eunzip_buffer:next_chunk(FileBuffer6, CommentLength),
            {ok, FileBuffer8} = eunzip_buffer:move_forward_by(FileBuffer7, CommentLength),
            Entry = #cd_entry{
                bit_flag = Flag,
                compression_method = CompressionMethod,
                last_modified_datetime = to_datetime(<<MDate:16>>, <<MTime:16>>),
                crc = Crc,
                compressed_size = CompressedSize,
                uncompressed_size = UncompressedSize,
                local_header_offset = LocalHeaderOffset,
                % TODO: we should treat binary as "IBM Code Page 437" encoded string if GP flag 11 is not set
                file_name = FileName
            },
            Entry1 = case need_zip64_extra(Entry) of
                true -> merge_zip64_extra(Entry, ExtraFields);
                false -> Entry
            end,
            case add_entry(Acc, FileName, Entry1) of
                {error, _} = Error -> Error;
                Acc1 -> parse_cd(FileBuffer8, Acc1)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

add_entry(#{entries := Entries, range_tree := RangeTree} = Acc, FileName, Entry) ->
    #cd_entry{local_header_offset = LocalHeaderOffset, compressed_size = CompressedSize} = Entry,
    case eunzip_range:is_overlap(RangeTree, LocalHeaderOffset, CompressedSize) of
        true -> {error, overlapped_zip_entries};
        false -> Acc#{entries => Entries#{FileName => Entry}, range_tree => eunzip_range:insert(RangeTree, LocalHeaderOffset, CompressedSize)}
    end.

to_datetime(<<Year:7, Month:4, Day:5>> = _Date, <<Hour:5, Minute:6, Second:5>> = _Time) ->
    {{1980 + Year, Month, Day}, {Hour, Minute, Second * 2}}.

need_zip64_extra(#cd_entry{compressed_size = Cs, uncompressed_size = Ucs, local_header_offset = Offset}) ->
    lists:any(fun(Elem) -> Elem =:= 16#ffffffff end, [Cs, Ucs, Offset]).

merge_zip64_extra(Entry, Extra) ->
    Zip64Extra = maps:get(?zip64_extra_field_id, find_extra_fields(Extra)),
    {Entry1, Zip64Extra1} = case Entry of
        #cd_entry{uncompressed_size = Ucs} when Ucs =:= 16#ffffffff ->
            <<UncompressedSize:64/little, Rest/binary>> = Zip64Extra,
            {Entry#cd_entry{uncompressed_size = UncompressedSize}, Rest};
        _ -> {Entry, Zip64Extra}
    end,
    {Entry2, Zip64Extra2} = case Entry1 of
        #cd_entry{compressed_size = Cs} when Cs =:= 16#ffffffff ->
            <<CompressedSize:64/little, Rest1/binary>> = Zip64Extra1,
            {Entry1#cd_entry{compressed_size = CompressedSize}, Rest1};
        _ -> {Entry1, Zip64Extra1}
    end,
    {Entry3, _Zip64Extra3} = case Entry2 of
        #cd_entry{local_header_offset = Offset} when Offset =:= 16#ffffffff ->
            <<LocalHeaderOffset:64/little, Rest2/binary>> = Zip64Extra2,
            {Entry2#cd_entry{local_header_offset = LocalHeaderOffset}, Rest2};
        _ -> {Entry2, Zip64Extra2}
    end,
    Entry3.

find_extra_fields(Extra) -> find_extra_fields(Extra, #{}).
find_extra_fields(<<>>, Result) -> Result;
find_extra_fields(<<Id:16/little, Size:16/little, Data:Size/binary, Rest/binary>>, Result) -> find_extra_fields(Rest, Result#{Id => Data}).

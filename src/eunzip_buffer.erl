%% @author: Maxim Pushkar
%% @date: 06.04.2020

-module(eunzip_buffer).

%% Include files
-include("eunzip.hrl").

%% API
-export([
    new/3,
    new/6,
    next_chunk/2,
    move_backward_by/2,
    move_forward_by/2
]).

%% API
-spec new(FileHandle, FileSize, BufferSize) -> Result when
    FileHandle :: file:fd(),
    FileSize :: non_neg_integer(),
    BufferSize :: non_neg_integer(),
    Result :: {'ok', eunzip_types:file_buffer()} | {'error', Reason :: atom()}.

new(FileHandle, FileSize, BufferSize) ->
    {ok, #file_buffer{
        file = FileHandle,
        size = FileSize,
        limit = FileSize,
        buffer = <<>>,
        buffer_position = FileSize,
        buffer_size = BufferSize,
        direction = backward
    }}.

-spec new(FileHandle, FileSize, BufferSize, Limit, Position, Direction) -> Result when
    FileHandle :: file:fd(),
    FileSize :: non_neg_integer(),
    BufferSize :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Position :: non_neg_integer(),
    Direction :: eunzip_types:direction(),
    Result :: {'ok', eunzip_types:file_buffer()} | {'error', Reason :: atom()}.

new(FileHandle, FileSize, BufferSize, Limit, Position, Direction) when Limit =< FileSize ->
    {ok, #file_buffer{
        file = FileHandle,
        size = FileSize,
        limit = Limit,
        buffer = <<>>,
        buffer_position = Position,
        buffer_size = BufferSize,
        direction = Direction
    }};

new(_FileHandle, _FileSize, _BufferSize, _Limit, _Position, _Direction) ->
    {error, invalid_limit}.

-spec next_chunk(FileBuffer, Size) -> Result when
    FileBuffer :: eunzip_types:file_buffer(),
    Size :: non_neg_integer(),
    Result :: {'ok', binary(), eunzip_types:file_buffer()} | {'error', Reason :: atom()}.

next_chunk(#file_buffer{direction = forward, file = File, buffer = Buffer, buffer_size = BufferSize, buffer_position = Pos, limit = Limit} = FileBuffer, Size) ->
    BufferEndPos = Pos + byte_size(Buffer),
    EndPos = min(Pos + Size, Limit),
    if
        EndPos > BufferEndPos ->
            NewBufferEndPos = min(Limit, max(BufferEndPos + BufferSize, EndPos)),
            ChunkSize = NewBufferEndPos - Pos,
            case pread(File, Pos + byte_size(Buffer), ChunkSize) of
                {ok, Binary} ->
                    Buffer1 = <<Buffer/binary, Binary/binary>>,
                    {ok, binary_part(Buffer1, 0, min(Size, byte_size(Buffer1))), FileBuffer#file_buffer{buffer = Buffer1}};
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            {ok, binary_part(Buffer, 0, min(Size, byte_size(Buffer))), FileBuffer}
    end;

next_chunk(#file_buffer{file = File, buffer = Buffer, buffer_size = BufferSize, buffer_position = Pos} = FileBuffer, Size) ->
    BufferEndPos = Pos + byte_size(Buffer),
    StartPos = max(0, BufferEndPos - Size),
    if
        Pos > StartPos ->
            NewBufferPos = max(0, min(Pos - BufferSize, StartPos)),
            ChunkSize = Pos - NewBufferPos,
            case pread(File, NewBufferPos, ChunkSize) of
                {ok, Binary} ->
                    Offset = StartPos - NewBufferPos,
                    Buffer1 = <<Buffer/binary, Binary/binary>>,
                    {ok, binary_part(Buffer1, Offset, min(Size, byte_size(Buffer1))), FileBuffer#file_buffer{buffer = Buffer1, buffer_position = NewBufferPos}};
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            Offset = StartPos - Pos,
            {ok, binary_part(Buffer, Offset, min(Size, byte_size(Buffer))), FileBuffer}
    end.

-spec move_backward_by(FileBuffer, Count) -> Result when
    FileBuffer :: unzip:file_buffer(),
    Count :: non_neg_integer(),
    Result :: {'ok', unzip:file_buffer()} | {'error', Reason :: atom()}.

move_backward_by(#file_buffer{buffer = Buffer}, Count) when byte_size(Buffer) < Count ->
    {error, invalid_count};

move_backward_by(#file_buffer{buffer = Buffer} = FileBuffer, Count) ->
    Buffer1 = binary_part(Buffer, 0, byte_size(Buffer) - Count),
    {ok, FileBuffer#file_buffer{buffer = Buffer1}}.

-spec move_forward_by(FileBuffer, Count) -> Result when
    FileBuffer :: unzip:file_buffer(),
    Count :: non_neg_integer(),
    Result :: {'ok', unzip:file_buffer()} | {'error', Reason :: atom()}.

move_forward_by(#file_buffer{buffer = Buffer}, Count) when byte_size(Buffer) < Count ->
    {error, invalid_count};

move_forward_by(#file_buffer{buffer = Buffer, buffer_position = Pos, limit = Limit} = FileBuffer, Count) ->
    Buffer1 = binary_part(Buffer, Count, byte_size(Buffer) - Count),
    {ok, FileBuffer#file_buffer{buffer = Buffer1, buffer_position = min(Pos + Count, Limit)}}.

%% Internal functions
pread(File, Offset, Length) ->
    case file:pread(File, Offset, Length) of
        {ok, Binary} when is_binary(Binary) -> {ok, Binary};
        {ok, _} -> {error, not_a_binary};
        {error, Reason} -> {error, Reason}
    end.

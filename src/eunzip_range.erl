%% @author: Maxim Pushkar
%% @date: 06.04.2020

-module(eunzip_range).

%% API
-export([
    new/0,
    insert/3,
    is_overlap/3
]).

%% API
-spec new() -> gb_trees:tree().

new() ->
    gb_trees:empty().

-spec insert(Tree, Offset, Length) -> Result when
    Tree :: gb_trees:tree(),
    Offset :: non_neg_integer(),
    Length :: non_neg_integer(),
    Result :: gb_trees:tree().

insert(Tree, Offset, Length) ->
    gb_trees:insert(Offset, Length, Tree).

-spec is_overlap(Tree, Offset, Length) -> Result when
    Tree :: gb_trees:tree(),
    Offset :: non_neg_integer(),
    Length :: non_neg_integer(),
    Result :: boolean().

is_overlap({_, TreeNode} = _Tree, Offset, Length) ->
    is_pos_overlap(TreeNode, Offset, nil) orelse is_pos_overlap(TreeNode, Offset + Length - 1, nil).

%% Internal functions
is_pos_overlap(nil, _, nil) ->
    false;
is_pos_overlap(nil, Pos, {Offset, Length, _, _}) ->
    Offset =< Pos andalso Pos < (Offset + Length - 1);
is_pos_overlap({Offset, _, Smaller, _}, Pos, PrevRange) when Offset > Pos ->
    is_pos_overlap(Smaller, Pos, PrevRange);
is_pos_overlap({_, _, _, Bigger} = Range, Pos, _) ->
    is_pos_overlap(Bigger, Pos, Range).

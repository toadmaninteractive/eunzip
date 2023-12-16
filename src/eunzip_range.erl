%% @author: Maxim Pushkar
%% @date: 16.12.2023

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

is_overlap(Tree, Offset, Length) ->
    Iter = gb_trees:iterator(Tree),
    iterate(gb_trees:next(Iter), Offset, Offset + Length - 1).

%% Internal functions
iterate(none = _Iter, _From, _To) -> false;
iterate({Offset, _Length, _Iter}, From, To) when From < Offset, To >= Offset -> true;
iterate({Offset, Length, _Iter}, From, _To) when From >= Offset, From < Offset + Length -> true;
iterate({_Offset, _Length, Iter}, From, To) -> iterate(gb_trees:next(Iter), From, To).

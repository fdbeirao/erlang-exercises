-module(ex6).
-export([quicksort/1, mergesort/1]).

%% Public API

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
  quicksort(items_smaller_then(Pivot, Rest)) ++ 
  [Pivot] ++ 
  quicksort(items_greater_or_equal_then(Pivot, Rest)).

mergesort([]) -> [];
mergesort([SingleItem]) -> [SingleItem];
mergesort(List) ->
  {LeftHalf, RightHalf} = lists:split(trunc(length(List)/2), List),
  mergesort(mergesort(LeftHalf), mergesort(RightHalf)).

%% Private functions

items_smaller_then(Value, List) ->
  items_smaller_then(Value, List, []).

items_smaller_then(_Value, [], Acc) -> Acc;
items_smaller_then(Value, [Head|Tail], Acc) when Head < Value ->
  items_smaller_then(Value, Tail, [Head|Acc]);
items_smaller_then(Value, [_Head|Tail], Acc) ->
  items_smaller_then(Value, Tail, Acc).

items_greater_or_equal_then(Value, List) ->
  items_greater_or_equal_then(Value, List, []).

items_greater_or_equal_then(_Value, [], Acc) -> Acc;
items_greater_or_equal_then(Value, [Head|Tail], Acc) when Head >= Value ->
  items_greater_or_equal_then(Value, Tail, [Head|Acc]);
items_greater_or_equal_then(Value, [_Head|Tail], Acc) ->
  items_greater_or_equal_then(Value, Tail, Acc).

mergesort(Left, []) -> Left;
mergesort([], Right) -> Right;
mergesort([LeftHead|LeftTail], [RightHead|RightTail]) when LeftHead < RightHead ->
  [LeftHead | mergesort(LeftTail, [RightHead|RightTail])];
mergesort([LeftHead|LeftTail], [RightHead|RightTail]) ->
  [RightHead | mergesort([LeftHead|LeftTail], RightTail)].
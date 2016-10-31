-module(ex5).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

%% Public API

filter(List, Value) -> 
  get_values_smaller_than(Value, List, []).

reverse(List) ->
  reverse(List, []).

concatenate(List) ->
  concatenate(List, []).

flatten(List) ->
  flatten(List, []).

%% Private functions

get_values_smaller_than(_Value, [], SmallerValues) ->
  SmallerValues;
get_values_smaller_than(Value, [CurrentValue|Tail], SmallerValues) when CurrentValue =< Value ->
  get_values_smaller_than(Value, Tail, SmallerValues ++ [CurrentValue]);
get_values_smaller_than(Value, [_|Tail], SmallerValues) ->
  get_values_smaller_than(Value, Tail, SmallerValues).

reverse([], Reversed) -> 
  Reversed;
reverse([Head|Tail], Reversed) ->
  reverse(Tail, [Head|Reversed]).

concatenate([], Concatenated) -> reverse(Concatenated);
concatenate([[]|Tail], Concatenated) -> concatenate(Tail, Concatenated);
concatenate([[InnerHead|InnerTail]|OutterTail], Concatenated) ->
  concatenate([InnerTail|OutterTail], [InnerHead|Concatenated]);
concatenate([Head|Tail], Concatenated) ->
  concatenate(Tail, [Head|Concatenated]).

flatten([], Res) -> Res;
flatten([[]|Tail], Res) -> concatenate([flatten(Tail)|Res]);
flatten([Head|Tail], Res) -> concatenate([flatten(Head)|flatten(Tail)] ++ Res);
flatten(Elm, Res) -> [Elm|Res].
-module(ex2).
-export([create/1, reverse_create/1]).

%% Public API

create(N) when N >= 0 ->
  create(N, []).

reverse_create(N) when N >= 0 ->
  reverse_create(N, []).

%% Private functions

create(0, List) -> 
  List;
create(N, List) -> 
  create(N-1, [N] ++ List).

reverse_create(0, List) ->
  List;
reverse_create(N, List) ->
  reverse_create(N-1, List ++ [N]).
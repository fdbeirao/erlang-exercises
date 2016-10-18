-module(perms).
-export([perms/1]).

%%% perms([1,2])   -> [[1,2],[2,1]]
%%% perms([1,2,3]) -> [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
perms([]) ->
  [[]];
perms([X|Xs]) ->
  [insert(X,As,Bs) || Ps <- perms(Xs),
                      {As, Bs} <- splits(Ps)].

%%% splits([1])     -> [{[],[1]}    ,{[1],[]}]
%%% splits([1,2])   -> [{[],[1,2]}  ,{[1],[2]}  ,{[1,2],[]}]
%%% splits([1,2,3]) -> [{[],[1,2,3]},{[1],[2,3]},{[1,2],[3]},{[1,2,3],[]}]
splits([]) ->
  [{[],[]}];
splits([X|Xs] = Ys) ->
  [{[], Ys} | [ {[X|As], Bs} || {As,Bs} <- splits(Xs)]].

%%% insert(1, [2,3], [4,5]) -> [2,3,1,4,5]
insert(X, As, Bs) ->
  lists:append([As, [X], Bs]). %% http://erlang.org/doc/man/lists.html#append-1
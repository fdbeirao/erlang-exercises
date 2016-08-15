-module(powers).

-export([raise/2]).

raise(X, N) when N >= 0 ->
    raise(X, N, 1).

raise(_, 0, Acc) ->
    Acc;
raise(X, N, Acc) ->
    raise(X, N-1, X * Acc).
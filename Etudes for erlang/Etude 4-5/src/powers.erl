-module(powers).

-export([raise/2, nth_root/2]).

raise(X, N) when N >= 0 ->
    raise(X, N, 1).

raise(_, 0, Acc) ->
    Acc;
raise(X, N, Acc) ->
    raise(X, N-1, X * Acc).

nth_root(X, N) when is_integer(N) ->
    nth_root(X, N, X / 2.0).

nth_root(X, N, Approx) ->
    io:format("Current guess is ~p~n", [Approx]),
    F = raise(Approx, N) - X,
    Fprime = N * raise(Approx, N-1),
    Next = Approx - F / Fprime,    
    Change = abs(Next - Approx),
    if
        Change < 1.0e-8 -> Next;
        true -> nth_root(X, N, Next)
    end.
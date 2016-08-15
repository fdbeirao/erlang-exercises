-module(dijkstra).

-export([gcd/2]).

gcd(M, M) ->
    M;
gcd(M, N) when M > N ->
    gcd(M-N, N);
gcd(M, N) ->
    gcd(M, N-M).

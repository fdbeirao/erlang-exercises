-module(ex1).
-export([sum/1, sum/2]).

%% Public API

sum(1) -> 
  1;
sum(N) when N > 1 -> 
  N + sum(N-1).

sum(N,N) -> 
  N;
sum(N,M) when N =< M ->
  M + sum(N, M-1).
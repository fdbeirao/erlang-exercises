-module(stats).

-export([minimum/1, maximum/1, range/1, mean/1, stdv/1]).

-spec(minimum([any()]) -> any()).
minimum([L|T]) -> minimum(T, L).

minimum([], Min) -> Min;
minimum([H|T], Min) when H < Min -> minimum(T, H);
minimum([_|T], Min) -> minimum(T, Min).

-spec(maximum([any()]) -> any()).
maximum([L|T]) -> maximum(T, L).

maximum([], Max) -> Max;
maximum([H|T], Max) when H > Max -> maximum(T, H);
maximum([_|T], Max) -> maximum(T, Max).

-spec(range([any()]) -> [any()]).
range(L) -> [ minimum(L), maximum(L) ].

-spec(mean([any()]) -> number()).
mean([]) -> 0;
mean(L) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / length(L).

-spec(stdv([any()]) -> number()).
stdv([]) -> 0;
stdv(L) ->
  N = length(L),
  {Sum, SumOfSquares} = lists:foldl(fun(X, {Sum, Squares}) -> { Sum + X, Squares + X*X } end, { 0, 0 }, L),
  math:sqrt(((N * SumOfSquares) - (Sum * Sum)) / (N * (N-1))).

% Meh, had implemented this before reading "Use lists:foldl/3 to calculate the sum and the sum of squares. 
% Bonus points if you can calculate both of them with one call to lists:foldl/3."
%get_sums(L) -> get_sums(L, {0, 0}).
%get_sums([], Acc) -> Acc;
%get_sums([H|T], {Sum, Squares}) -> get_sums(T, {Sum + H, Squares + H*H}). 
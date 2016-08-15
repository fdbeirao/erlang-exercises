-module(stats).

-export([minimum/1, maximum/1, range/1]).

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

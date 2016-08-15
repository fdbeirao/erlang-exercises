-module(stats).

-export([minimum/1]).

-spec(minimum([any()]) -> any()).
minimum([L|T]) ->
    minimum(T, L).

minimum([], Min) ->
    Min;
minimum([H|T], Min) when H < Min ->
    minimum(T, H);
minimum([_|T], Min) ->
    minimum(T, Min).
-module(dates).

-export([date_parts/1]).

-spec(date_parts(string()) -> { number(), number(), number()}).
date_parts(Date) when length(Date) =:= 10 ->
    [Year, Month, Day] = re:split(Date, "[-]", [{return, list}]),
    { 
        to_int(Year), 
        to_int(Month), 
        to_int(Day) 
    }.

-spec(to_int(string()) -> integer()).
to_int(I) ->
    {N,_} = string:to_integer(I),
    N.
-module(dates).

-export([date_parts/1, julian/1]).

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

-spec(julian(string()) -> integer()).
julian(Date) ->
    {Year, Month, Day} = date_parts(Date),
    DaysInMonth = daysPerMonth(Year),
    julian(Month - 1, DaysInMonth, Day). 

julian(0, _, Acc) -> Acc;
julian(Month, [DaysInMonth|OtherMonths], Acc) ->
    julian(Month - 1, OtherMonths, Acc+DaysInMonth).

-spec(daysPerMonth(integer()) -> [integer()]).
daysPerMonth(Year) ->
    case is_leap_year(Year) of
        true ->  [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        false -> [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    end.

is_leap_year(Year) ->
    (Year rem 4 == 0 andalso Year rem 100 /= 0)
    orelse (Year rem 400 == 0).
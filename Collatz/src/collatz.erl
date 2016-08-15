-module(collatz).

-export([calc/1]).

calc(C) -> calc(C, []).

calc(1, Acc) -> lists:reverse([1|Acc]);
calc(C, Acc) when C rem 2 =:= 0 -> calc(C div 2, [C|Acc]);
calc(C, Acc) -> calc((3*C+1), [C|Acc]).
-module(ex74).
-include("records.hrl").
-export([perimeter/1, area/1]).

perimeter(#circle{radius=R}) -> 2 * math:pi() * R;
perimeter(#rectangle{length=L, width= W}) -> 2* (L + W).

area(#circle{radius=R}) -> math:pi() * R * R;
area(#rectangle{length=L, width=W}) -> L * W.
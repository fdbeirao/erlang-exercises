-module(ex74).
-include("records.hrl").
-export([perimeter/1, area/1]).

perimeter(#circle{radius=R}) -> 2 * math:pi() * R;
perimeter(#rectangle{length=L, width= W}) -> 2* (L + W);
perimeter(#triangle{side1=A, side2=B, side3=C}) -> A+B+C.

area(#circle{radius=R}) -> math:pi() * R * R;
area(#rectangle{length=L, width=W}) -> L * W;
area(#triangle{side1=A, side2=B, side3=C}) -> 
  math:sqrt((A+B-C)*(A-B+C)*(-A+B+C)*(A+B+C))/4. 
  %% Source: https://www.wolframalpha.com/input/?i=triangle+area
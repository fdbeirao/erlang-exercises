-module(geom).

-export([area/3]).

area(rectangle, Width, Height) ->
    Width * Height;
area(triangle, Base, Height) ->
    (Base * Height) / 2.0;
area(ellipse, MajorRadius, MinorRadius) ->
    math:pi() * MajorRadius * MinorRadius.

-module(part_two).

-export([people/0, separate/0]).

people() -> [{"Federico", $M, 22}, 
             {"Kim", $F, 45}, 
             {"Hansa", $F, 30},
             {"Tran", $M, 47}, 
             {"Cathy", $F, 32}, 
             {"Elias", $M, 50}].

separate() ->
  [Name || {Name, Gender, Age} <- people(), Gender =:= $M orelse Age > 40].
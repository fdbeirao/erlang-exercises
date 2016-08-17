-module(non_fp).

-export([generate_teeth/2]).

generate_teeth(Teeth, ProbGood) ->
  generate_teeth(Teeth, ProbGood, []).
    
generate_teeth([], _, Teeth) -> 
  lists:reverse(Teeth);
generate_teeth([$F|Rest], ProbGood, Teeth) -> 
  generate_teeth(Rest, ProbGood, [[0]|Teeth]);
generate_teeth([$T|Rest], ProbGood, Teeth) ->
  Tooth = generate_tooth(ProbGood), 
  generate_teeth(Rest, ProbGood, [Tooth|Teeth]).

generate_tooth(ProbGood) ->
    [getReading(ProbGood) rem 5 || _ <- lists:seq(1,6)].

getReading(ProbGood) ->
    ToothReading = rand:uniform(),
    case ToothReading < ProbGood of
        true -> rand:uniform(3);
        false -> 4
    end.

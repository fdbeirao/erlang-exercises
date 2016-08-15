-module(non_fp).

%-export([generate_teeth/3]).
-compile(export_all).

%generate_teeth() ->
    


generate_tooth(ProbGood) ->
    [getReading(ProbGood) rem 5 || _ <- lists:seq(1,6)].

getReading(ProbGood) ->
    ToothReading = rand:uniform(),
    case ToothReading < ProbGood of
        true -> rand:uniform(3);
        false -> 4
    end.

-module(teeth).

-export([alert/1]).

-spec(alert([[integer()]]) -> [integer()]).
alert(Teeth) -> extract_alerts(1, Teeth, []).

extract_alerts(_, [], Alerts) -> 
    lists:reverse(Alerts);
extract_alerts(Index, [Tooth|Teeth], Alerts) ->
    case requires_attention(Tooth) of
        true -> extract_alerts(Index+1, Teeth, [Index|Alerts]);
        false -> extract_alerts(Index+1, Teeth, Alerts)
    end.

requires_attention(Measurements) ->
    lists:any(fun(A) -> A >= 4 end, Measurements).
-module(carrot).

-export([create_carrot/0]).
-export([alive/1]).

create_carrot() ->
  spawn(?MODULE, alive, [1]).

alive(CurrentSize) ->
  receive
    { grow, 5 } ->
      io:fwrite("carrot has perished~n"),
      perish;
    { grow, NewSize } ->
      io:fwrite("carrot has grown to ~p~n", [NewSize]),
      alive(NewSize);
    { eat, ByWhom } ->
      ByWhom ! { eaten, CurrentSize }
  after 1000 ->
    self() ! { grow, CurrentSize + 1 },
    alive(CurrentSize)
  end.
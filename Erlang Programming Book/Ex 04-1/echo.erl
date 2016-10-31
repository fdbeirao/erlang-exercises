-module(echo).
-export([start/0, print/1, stop/0, echo_loop/0]).

%% Public module API

start() ->
  %% This does not guarantee that only one spawn of the echo server occurrs
  %% Yet, there is nothing in the Exercise stating that we should consider
  %% concurrency at this point
  case whereis(?MODULE) of
    undefined -> register(?MODULE, spawn(?MODULE, echo_loop, [])),
    _ -> true
  end,
  ok.

print(Msg) ->
  ?MODULE ! {print, Msg},
  ok.

stop() ->
  ?MODULE ! stop,
  ok.

%% Echo process functions

echo_loop() ->
  receive
    {print, Msg} ->
      io:fwrite("~p~n", [Msg]), 
      echo_loop();
    stop -> ok
  end.
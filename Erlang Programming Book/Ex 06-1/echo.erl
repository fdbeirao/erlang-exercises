-module(echo).
-export([start/0, print/1, stop/0]).
-export([proc_a_init/0, proc_b_init/0]).

%% Public module API

start() ->
  register(procA, spawn(?MODULE, proc_a_init, [])),
  ok.

print(Msg) ->
  procA ! {print, Msg},
  ok.

stop() ->
  PidA = whereis(procA),
  exit(PidA, abnormal),
  ok.

%% Echo process functions

proc_a_init() ->
  register(procB, spawn_link(?MODULE, proc_b_init, [])),
  proc_a_loop().

proc_a_loop() ->
  receive
    {print, Msg} ->
      procB ! Msg,
      proc_a_loop()
  end.

proc_b_init() ->
  proc_b_loop().

proc_b_loop() ->
  receive
    Msg ->
      io:fwrite("~p~n", [Msg]),
      proc_b_loop()
  end.

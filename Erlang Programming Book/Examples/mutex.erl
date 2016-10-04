-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

%%%%%% /!\ DO NOT USE THIS CODE /!\
%%%%%% IT DOES NOT LINK TO THE CALLING PROCESS, SO IF THE PROCESS DIES, 
%%%%%% THE STATE MACHINE WILL NOT CHANGE FROM BUSY TO FREE
%%%%%% AND YOUR NEW PROCESS WILL HANG IN A RECEIVE.

%% Public API functions

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  ?MODULE ! stop.

wait() ->
  ?MODULE ! {wait, self()},
  receive ok -> ok end.

signal() ->
  ?MODULE ! {signal, self()}, 
  ok.

%% Mutex process functions

init() ->
  free().

free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop ->
      terminate()
  end.

busy(Pid) ->
  receive
    {signal, Pid} ->
      free()
  end.

terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after 
    0 -> ok
  end.
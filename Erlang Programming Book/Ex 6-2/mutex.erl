-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
  global:register_name(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  global:send(?MODULE, stop).

wait() ->
  global:send(?MODULE, {wait, self()}),
  receive ok -> ok end.

signal() ->
  global:send(?MODULE, {signal, self()}), 
  ok.

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
  after 0 ->
    ok
  end.
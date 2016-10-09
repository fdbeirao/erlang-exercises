-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
  global:register_name(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  global:send(?MODULE, stop).

wait() ->
  MutexPid = global:whereis_name(?MODULE),
  MonitorRef = erlang:monitor(process, MutexPid),
  global:send(?MODULE, {wait, self()}),
  receive 
    ok -> ok;
    {'DOWN', MonitorRef, process, _Object, _Info} -> exit(self(), mutex_not_available) 
  end.

signal() ->
  global:send(?MODULE, {signal, self()}), 
  ok.

init() ->
  process_flag(trap_exit, true),
  free().

free() ->
  receive
    {wait, Pid} ->
      MonitorRef = erlang:monitor(process, Pid),
      Pid ! ok,
      busy(Pid, MonitorRef);
    stop ->
      terminate()
  end.

busy(Pid, MonitorRef) ->
  receive
    {signal, Pid} ->
      erlang:demonitor(MonitorRef),
      free();
    {'DOWN', MonitorRef, process, _Object, _Info} ->
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
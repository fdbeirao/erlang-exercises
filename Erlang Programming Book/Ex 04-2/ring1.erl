-module(ring1).
-export([start/3, ring_proc/1]).

%% Following strategy 1: have a central process that sets up the ring and initiates sending the message

%% Public module API

start(M, N, Msg) ->
  Procs = create_processes(N),
  [HeadProc|_] = Procs,
  join_ring_processes(HeadProc, Procs),
  HeadProc ! { print, M, Msg },
  ok.

%% Private module functions

create_processes(M) -> 
  create_processes(M, []).

create_processes(0, Procs) -> 
  Procs;
create_processes(M, Procs) ->
  create_processes(M-1, [spawn(?MODULE, ring_proc, [{wait_for_ring, M}])|Procs]).

join_ring_processes(HeadProc, [CurrProc|[NextProc|RemainingProcs]]) ->
  CurrProc ! { set_next_element, NextProc },
  join_ring_processes(HeadProc, [NextProc|RemainingProcs]);
join_ring_processes(HeadProc, [LastProc]) ->
  LastProc ! { set_next_element, HeadProc }.

%% Ring process functions

ring_proc({wait_for_ring, ProcIndex}) ->
  receive
    {set_next_element, NextProcessPid} -> 
      io:format("Proc [~p ~p] received [~p] as next element in ring~n", [ProcIndex, self(), NextProcessPid]),
      ring_proc(NextProcessPid, ProcIndex)
  end.

ring_proc(NextProcessPid, ProcIndex) ->
  receive
    {print, 0, _Msg} ->
      self() ! terminate,
      ring_proc(NextProcessPid, ProcIndex);
    {print, MsgHops, Msg} -> 
      io:format("Proc [~p ~p]> ~p~n", [ProcIndex, self(), Msg]),
      NextProcessPid ! {print, MsgHops - 1, Msg},
      ring_proc(NextProcessPid, ProcIndex);
    terminate ->
      NextProcessPid ! terminate,
      io:format("Proc [~p ~p] terminating~n", [ProcIndex, self()])
  end.
-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(?MODULE, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M, F, A, T} | ChildSpecList]) ->
  [start_child(M, F, A, T, 0, fun now/0)|start_children(ChildSpecList)].

start_child(M, F, A, T, R, Now) ->
  {ok, Pid} = apply(M, F, A),
  {Pid, {M, F, A}, T, R + 1, Now()}.

now() ->
  erlang:monotonic_time(seconds).

try_restart({Pid, {M, F, A}, T, R, LRT}, ChildList, Now) when T == permanent andalso R =< 5 ->
  TimeDiff = Now() - LRT,
  if 
    TimeDiff =< 5 ->
      [start_child(M, F, A, T, R, Now)|remove_child(Pid, ChildList)];
    true -> 
      remove_child(Pid, ChildList)
  end;

try_restart({Pid, {_M, _F, _A}, _T, _R, _LRT}, ChildList, _Now) ->
  remove_child(Pid, ChildList).

child_died(Pid, ChildList) ->
  {value, Child} = lists:keysearch(Pid, 1, ChildList),
  try_restart(Child, ChildList, fun now/0).

remove_child(Pid, ChildList) ->
  lists:keydelete(Pid, 1, ChildList).

loop(ChildList) ->
  receive
    {'EXIT', Pid, _Reason } ->
      NewChildList = child_died(Pid, ChildList),
      loop(NewChildList);
    {stop, From} ->
      From ! {reply, terminate(ChildList)}
  end.

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.
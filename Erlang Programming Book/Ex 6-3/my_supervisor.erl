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
  case (catch apply(M, F, A)) of
    {ok, Pid} ->
      [{Pid, {M, F, A, T}}|start_children(ChildSpecList)];
    _ ->
      start_children(ChildSpecList)
  end.

child_died(Pid, ChildList) ->
  {value, {Pid, {M, F, A, T}}} = lists:keysearch(Pid, 1, ChildList),
  case T of
    transient -> 
      remove_child(Pid, ChildList);
    permanent -> 
      {ok, NewPid} = apply(M, F, A),
      [{NewPid, {M,F,A}}|remove_child(Pid, ChildList)]
  end.

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
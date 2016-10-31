-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

init() -> Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The client functions

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.

call(Message) ->
  ?MODULE ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

%% The Main loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      case any_allocated_frequency(Frequencies) of
        false -> reply(Pid, ok);
        true  -> reply(Pid, {error, allocated_frequencies}), loop(Frequencies)
      end
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency_available}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case allocatedFrequenciesForPid(Pid, Allocated) >= 3 of
    true  -> {{[Freq|Free], Allocated}, {error, exceeded_limit}};
    false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  NewAllocated = lists:delete({Freq, Pid}, Allocated),
  case length(NewAllocated) < length(Allocated) of
    true  -> {{[Freq|Free], NewAllocated}, ok};
    false -> {{Free, Allocated},{error, unauthorized}}
  end.

any_allocated_frequency({_, []}) -> false;
any_allocated_frequency(_)       -> true.

allocatedFrequenciesForPid(Pid, Allocated) ->
  length(lists:filter(fun(Elm) -> element(2, Elm) =:= Pid end, Allocated)).
-module(my_db).
-include("my_db_tuples.hrl").
-export([start/0, stop/0]). % Admin API
-export([write/2, delete/1, read/1, match/1]). % Public API
-export([init/0]). % Process API

%% Admin API

start() ->
  register(?MODULE, spawn(?MODULE, init, [])), ok.

stop() ->
  ?MODULE ! {stop, self() }, 
  receive {reply, Reply} -> Reply end.

%% Public API

write(Key, Element) ->
  call({write, #data{key=Key, value=Element}}).

delete(Key) ->
  call({delete, Key}).

read(Key) ->
  call({read, Key}).

match(Element) ->
  call({match, Element}).

%% Process API

handle_command({write, Data}, State) ->
  {ok, lists:keystore(Data#data.key, #data.key, State, Data)};

handle_command({delete, Key}, State) ->
  {ok, lists:keydelete(Key, #data.key, State)};

handle_command({read, Key}, State) ->
  FindResult = case lists:keyfind(Key, #data.key, State) of
    false -> {error, instance};
    Result -> {ok, Result#data.value}
  end,
  {FindResult, State};

handle_command({match, Element}, State) ->
  Keys = [X#data.key || X <- State, X#data.value =:= Element],
  {Keys, State}.

init() -> 
  loop([]).

call(Command) ->
  ?MODULE ! {request, self(), Command},
  receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
  To ! {reply, Msg}.

loop(State) ->
  receive
    {request, From, Command} ->
      {Reply, NewState} = handle_command(Command, State),
      reply(From, Reply),
      loop(NewState);
    {stop, From } ->
      reply(From, ok)
  end.
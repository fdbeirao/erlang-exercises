-module(my_db).
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
  call({write, Key, Element}).

delete(Key) ->
  call({delete, Key}).

read(Key) ->
  call({read, Key}).

match(Element) ->
  call({match, Element}).

%% Process API

handle_command({write, Key, Element}, State) ->
  {ok, lists:keystore(Key, 1, State, {Key, Element})};

handle_command({delete, Key}, State) ->
  {ok, lists:keydelete(Key, 1, State)};

handle_command({read, Key}, State) ->
  FindResult = case lists:keyfind(Key, 1, State) of
    {Key, Element} -> {ok, Element};
    false -> {error, instance}
  end,
  {FindResult, State};

handle_command({match, Element}, State) ->
  Filtered = lists:filter(fun(Item) -> element(2, Item) =:= Element end, State),
  Map = lists:map(fun(Item) -> element(1, Item) end, Filtered),
  {Map, State}.

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
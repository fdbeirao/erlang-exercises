-module(url_store).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-define(SERVER, ?MODULE).
-record(short_url,
	{minURL,
	 fullURL,
	 created_on}).
-record(state, {}).
-compile([export_all]).

%% Public API exports
-export([start_link/0, store_url/2, get_url/1, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store_url(MinURL, FullURL) ->
    gen_server:call(?SERVER, {save_url, MinURL, FullURL}).

get_url(MinURL) ->
    case gen_server:call(?SERVER, {get_url, MinURL}) of
        {ok, FullURL} ->
            FullURL;
        not_found ->
            not_found
    end.

shutdown() ->
  gen_server:call(?SERVER, stop).

%% gen_server interesting callbacks

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?SERVER, self()]), %% TODO: find how to do proper logging in Erlang
  url_store:init_store(),
  {ok, #state{}}.

handle_call({save_url, MinURL, FullURL}, _From, State) ->
  url_store:insert_url(MinURL, FullURL),
  {reply, ok, State};

handle_call({get_url, MinURL}, _From, State) ->
  FullURL = url_store:query_url(MinURL),
  {reply, {ok, FullURL}, State};

handle_call(stop, _From, State) ->
  mnesia:stop(), %% TODO: should I be stopping mnesia here? Doesn't feel alright
  {stop, normal, State};

handle_call(_Request, _From, State) ->
  {reply, ignored_message, State}.

%% Internal functions
insert_url(MinURL, FullURL) ->
  Trans = fun() ->
	  CreatedOn = calendar:universal_time(),
	  mnesia:write(#short_url{minURL=MinURL, fullURL=FullURL, created_on=CreatedOn}) 
  end,
  mnesia:transaction(Trans).

query_url(MinURL) ->
  Trans = fun() ->
    Query = qlc:q([Row || Row <- mnesia:table(short_url), Row#short_url.minURL =:= MinURL]),
    Result = qlc:e(Query),
    lists:map(fun(URL) -> URL#short_url.fullURL end, Result)
  end,
  {atomic, Result} = mnesia:transaction(Trans),
  Result.

init_store() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(short_url, type)
  catch
    exit: _ ->
      mnesia:create_table(short_url, [{attributes, record_info(fields, short_url)},
					 {type, set},
					 {disc_copies, [node()]}])
  end.

%% gen_server default callbacks
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
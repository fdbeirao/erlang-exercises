-module(get_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{Url, Req2} = cowboy_req:binding(url, Req),	
	{ok, Req3} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/plain">>}],
		Url,
	 	Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

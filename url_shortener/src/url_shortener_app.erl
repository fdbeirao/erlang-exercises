-module(url_shortener_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  %mnesia:start(),
  Dispatch = cowboy_router:compile([
		{'_', [{"/:url", get_handler, []}]}
	]),
	cowboy:start_http(my_http_listener, 100, [{port, 9090}],
		[{env, [{dispatch, Dispatch}]}]
	),
  url_shortener_sup:start_link().

stop(_State) ->
  ok.
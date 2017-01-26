-module(mire_server).

% Public API
-export([start/1, stop/0]).

-define(MIRE_SERVER_NAME, mire_server).

% Public API

start(Port) ->
  tcp_server:start_link(?MIRE_SERVER_NAME, Port, { socket_handler, init }).

stop() ->
  tcp_server:stop(?MIRE_SERVER_NAME).

-module(echo_handler).

% Public API
-export([start/1, stop/0]).
% Export for tcp_server callback
-export([loop/1]).

-define(ECHO_SERVER_NAME, echo_server).

% Public API

start(Port) ->
  tcp_server:start_link(?ECHO_SERVER_NAME, Port, { ?MODULE, loop }).

stop() ->
  tcp_server:stop(?ECHO_SERVER_NAME).

% tcp_server callback

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    { ok, Data } ->
      gen_tcp:send(Socket, Data),
      loop(Socket);
    { error, closed } ->
      ok
  end.
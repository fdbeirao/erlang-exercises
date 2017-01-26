-module(echo).
%% http://20bits.com/article/network-programming-in-erlang
-author('Jesse E.I. Farmer <jesse@20bits.com>').

-export([listen/1]).

%% http://erlang.org/doc/man/gen_tcp.html#type-listen_option
%% http://erlang.org/doc/man/gen_tcp.html#type-option
-define(TCP_OPTIONS, [binary, {packet, line}, {active, false}, {reuseaddr, true}]).

%%%
%%% Public API
%%%

% Call echo:listen(Port) to start the service
listen(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  accept(LSocket).

%%%
%%% Private functions
%%%

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  socket_handler:init(Socket),
  accept(LSocket).

% Echo back whatever data we receive on Socket
loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      gen_tcp:send(Socket, Data),
      loop(Socket);
    {error, closed} ->
      ok
  end.
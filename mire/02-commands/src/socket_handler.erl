-module(socket_handler).

% Export for tcp_server callback
-export([init/1]).
% Export for socket_receiver
-export([receiver_loop/2]).

-define(PROMPT, "> ").

% tcp_server callback

init(Socket) ->
  init_receiver(Socket, self()),
  to_socket(Socket, ?PROMPT),
  loop(Socket).

loop(Socket) ->
  receive
    { from_socket, Message } ->
      self() ! { to_socket, Message },
      loop(Socket);
    { to_socket, Message } ->
      to_socket(Socket, Message),
      to_socket(Socket, ?PROMPT),
      loop(Socket);
    { error, Reason } ->
      io:format("Error. Reason: [~p]~n", [Reason])
  end.

to_socket(Socket, Data) ->
  gen_tcp:send(Socket, Data).

% socket_receiver functions

init_receiver(Socket, Pid) ->
  spawn(?MODULE, receiver_loop, [Socket, Pid]).

receiver_loop(Socket, Pid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      Pid ! { from_socket, Data },
      receiver_loop(Socket, Pid);
    {error, Reason} ->
      Pid ! { error, Reason }
  end.
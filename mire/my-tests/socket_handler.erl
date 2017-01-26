-module(socket_handler).

-export([init/1]).
-export([init_loop/1, receiver_loop/2]).

-define(PROMPT, "> ").

%%%
%%% Public API
%%%

init(Socket) ->
  spawn(?MODULE, init_loop, [Socket]).

%%%
%%% Private API
%%%

init_loop(Socket) ->
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
-module(socket_handler).

% Export for tcp_server callback
-export([init/1]).
% Export for socket_receiver
-export([receiver_loop/2]).

-define(PROMPT, "> ").

-record(state, { socket, mirePid }).

% tcp_server callback

init(Socket) ->
  MirePid = init_mire_statem(),
  issue_command(self(), "look"),
  init_receiver(Socket, self()),
  loop(#state{ socket = Socket, mirePid = MirePid }).

loop(#state{ socket = Socket, mirePid = MirePid } = State) ->
  receive
    { from_socket, RawInput } ->
      { Command, Args } = parse_input(RawInput),
      CommandResult = mire_statem:execute(MirePid, Command, Args),
      case CommandResult of
        { reply, Reply } ->
          reply(Socket, Reply),
          show_prompt(Socket),
          loop(State);
        { exit, Message } ->
          reply(Socket, Message)
      end;
    { error, Reason } ->
      io:format("Error. Reason: [~p]~n", [Reason])
  end.

reply(Socket, Message) ->
  to_socket(Socket, Message),
  to_socket(Socket, "\r\n").

show_prompt(Socket) ->
  to_socket(Socket, ?PROMPT).

to_socket(Socket, Data) ->
  gen_tcp:send(Socket, Data).

init_mire_statem() ->
  mire_statem:start().

parse_input(RawInput) ->
  CleanInput = re:replace(RawInput, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
  [Command|Args] = string:tokens(CleanInput, " "),
  { Command, Args }.

issue_command(Pid, Command) ->
  Pid ! { from_socket, Command }.

% socket_receiver functions

init_receiver(Socket, Pid) ->
  spawn(?MODULE, receiver_loop, [Socket, Pid]).

receiver_loop(Socket, Pid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      issue_command(Pid, Data),
      receiver_loop(Socket, Pid);
    {error, Reason} ->
      Pid ! { error, Reason }
  end.
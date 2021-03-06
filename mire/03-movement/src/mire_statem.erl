-module(mire_statem).
-behaviour(gen_statem).

% Public API exports
-export([start/0, execute/3, stop/1]).
% gen_statem callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
% state callbacks
-export([running/3]).

-define(STATEM_OPTIONS, []).

-record(data, { room }).

%% Public API

start() ->
  { ok, Pid } = gen_statem:start(?MODULE, [], ?STATEM_OPTIONS),
  Pid.

execute(Pid, Command, Args) ->
  gen_statem:call(Pid, { execute, Command, Args }).

stop(Pid) ->
  gen_statem:stop(Pid).

%% gen_statem callbacks
terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  { ok,State,Data }.

init([]) ->
  State = running, 
  Data = #data{ room = rooms:initial_room_id() },
  { ok, State, Data }.

callback_mode() -> 
  state_functions.

%% state callbacks

running({ call, From }, { execute, "exit", _Args }, _Data) ->
  { stop_and_reply, normal, [exit_reply(From, "Goodbye")] };

running({ call, From }, { execute, "time", _Args }, Data) ->
  get_current_time(),
  { keep_state, Data, [normal_reply(From, get_current_time())] };

running({ call, From }, { execute, "look", _Args }, #data{ room=CurrentRoom } = Data) ->
  RoomDescription = rooms:get_room_description(CurrentRoom),
  RoomExits = rooms:get_room_exits(CurrentRoom),
  Reply = io_lib:format("~s\r\nPossible exits: ~p", [RoomDescription, RoomExits]),
  { keep_state, Data, [normal_reply(From, Reply)] };

running(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({ call, From }, { execute, Command, _Args }, Data) ->
  InvalidCommandReply = io_lib:format("Unknown command ~p", [Command]),
  { keep_state, Data, [normal_reply(From, InvalidCommandReply)]};

%% Ignore all other events
handle_event(_, _, Data) ->
    { keep_state, Data }.

%% These two are a contract with socket_handler
exit_reply(To, Message) ->
  { reply, To, { exit, Message } }.

normal_reply(To, Message) ->
  { reply, To, { reply, Message } }.

get_current_time() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(erlang:timestamp()),
  io_lib:format("UTC time: ~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]).
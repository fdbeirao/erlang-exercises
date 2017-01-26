%% This module is the server that opens a TCP port and listens on that TCP port

-module(tcp_server).
-behaviour(gen_server).

% Public API
-export([start_link/3, stop/1]).
% gen_server functions
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
% Inner process (socket acceptor) exports
-export([accept_socket/1]).

-define(SERVER_OPTIONS, []).
-define(TCP_OPTIONS, [binary, { packet, line }, { active, false }, { reuseaddr, true }]).

-record(state, { port, socketReceiveLoop, listeningSocket = null, serverName }).

%%% Public API

start_link(Name, Port, SocketReceiveLoop) ->
  State = #state{ port = Port, socketReceiveLoop = SocketReceiveLoop, serverName = Name },
  gen_server:start_link({ local, Name }, ?MODULE, State, ?SERVER_OPTIONS).

stop(Name) ->
  gen_server:cast(Name, stop).

%%% gen_server

init(#state{ port = Port } = State) ->
  process_flag(trap_exit, true),
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    { ok, LSocket } ->
      NewState = State#state{ listeningSocket = LSocket },
      spawn_socket_acceptor(NewState),
      { ok, NewState };
    { error, Reason } ->
      { stop, Reason }
  end.

% This cast comes from the socket acceptor. It signals this server to spawn a new acceptor
% for the listening socket
handle_cast({ accepted }, State) ->
  spawn_socket_acceptor(State),
  { noreply, State };

handle_cast(stop, State) ->
  { stop, normal, State };

handle_cast(OtherCast, #state{ serverName = Name } = State) ->
  io:format("Unexpected CAST to server ~p: [~p]~n", [Name, OtherCast]),
  State.

handle_info({ 'EXIT', _Pid, _Reason }, State) ->
  { noreply, State };

handle_info(OtherInfo, #state{ serverName = Name } = State) -> 
  io:format("Unexpected INFO to server ~p: [~p]~n", [Name, OtherInfo]),
  { noreply, State }.

terminate(_Reason, _State) -> 
  ok.

handle_call(Msg, Caller, #state{ serverName = Name } = State) -> 
  io:format("Unexpected CALL from ~p to server ~p: [~p]~n", [Caller, Name, Msg]),
  { noreply, State }.

code_change(_OldVersion, State, _Extra) -> 
  { ok, State }.

%%% Private functions

spawn_socket_acceptor(#state{ listeningSocket = LSocket, socketReceiveLoop = RLoop }) ->
  ServerPid = self(),
  spawn_link(?MODULE, accept_socket, [{ ServerPid, LSocket, RLoop }]).

% accept_socket is running in its own process, because gen_tcp:accept is synchronous
% It communicates back to the main server when it accepts a connection, so that the
% server can spawn a new acceptor
accept_socket({ Server, LSocket, { Module, Function } }) ->
  case gen_tcp:accept(LSocket) of
    { ok, Socket } ->
      gen_server:cast(Server, { accepted }),
      Module:Function(Socket);
    { error, closed } ->
      ok
  end.
-module(match).
-include("match.hrl");
-behavior(gen_fsm).

%% Public API
-export([start/1, start_link/1]).

%% Public API

start(Player1) ->
  gen_fsm:start(?MODULE, [Player], []).

start_link(Player1) ->
  gen_fsm:start_link(?MODULE, [Player], []).

%% Exported client functions

join(MatchPid, Player2) ->
  gen_fsm:send_event(MatchPid, { join, Player }).

terminate(MatchPid) ->
  gen_fsm:send_event(MatchPid, )

%% Callback functions

init(Player1) ->
  {ok, waiting_for_player2, #match{player1=Player1}}.

waiting_for_player2({join, Player2}, State=#match{}) ->
  notice(State, "player ~p joined match", [Player2]),
  {next_state, ongoing, State#match{player2=Player2}};
waiting_for_player2(Event, State) ->
  unexpected(Event, waiting_for_player2),
  {next_state, waiting_for_player2, State}.

ongoing()

%%% Utility functions
%% Send players a notice. This could be messages to their clients
%% but for our purposes, outputting to the shell is enough
notice(#state{name=N}, Str, Args) ->
  io:format("~s: "++Str++"~n", [N|Args]).

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).
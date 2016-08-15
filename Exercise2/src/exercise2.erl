-module(exercise2).

-export([start/1, waiting/0, working/1]).

waiting() ->
    io:format("~p has started~n", [self()]),
    receive
        finished ->
            io:format("~p has finished~n", [self()]);
        { nextNodeInRing, NextNode } ->
            io:format("~p has ~p as his next node~n", [self(), NextNode]),
            working(NextNode);
        OtherMsg ->
            io:format("Debug: ~p received ~p while waiting~n", [self(), OtherMsg]),
            waiting()
    end.

working(NextNode) ->
    receive
        finished ->
            io:format("~p has finished~n", [self()]),
            NextNode ! finished;
        {forward, 0} ->
            NextNode ! finished,
            working(NextNode);
        {forward, Times} ->
            io:format("~p has received { forward, ~p }~n", [self(), Times]),
            NextNode ! {forward, Times-1},
            working(NextNode);
        OtherMsg ->
            io:format("Debug: ~p received ~p while working~n", [self(), OtherMsg]),
            working(NextNode)
    end.

start(RingSize) when RingSize > 1 ->
    [First|Ring] = spawn_ring(RingSize, []),
    set_next(First, [First|Ring]),
    First.

spawn_ring(0, Ring) ->
    Ring;
spawn_ring(Remaining, Ring) ->
    Node = spawn(?MODULE, waiting, []),
    spawn_ring(Remaining - 1, [Node|Ring]).


set_next(FirstNode, [CurrentNode, LastNode]) ->
    CurrentNode ! { nextNodeInRing, LastNode },
    LastNode ! { nextNodeInRing, FirstNode };
set_next(FirstNode, [CurrentNode, NextNode|RemainingNodes]) ->
    CurrentNode ! { nextNodeInRing, NextNode },
    set_next(FirstNode, [NextNode|RemainingNodes]).
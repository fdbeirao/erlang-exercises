-module(ms).

% Public API
-export([start/1, to_slave/2]).
% Private API
-export([init_master/1, slave/1]).

-define(SERVER, master).

% API
start(TotalSlaves) when TotalSlaves > 0 ->
    ServerPid = spawn(?MODULE, init_master, [TotalSlaves]),
    register(?SERVER, ServerPid).

to_slave(Message, SlaveNumber) ->
    ?SERVER ! { Message, SlaveNumber }.

% Private methods

spawn_slaves(Total, Total, Slaves) ->
    Slaves;
spawn_slaves(Current, Total, Slaves) ->
    spawn_slaves(Current + 1, Total, spawn_slave(Current+1, Slaves)).

spawn_slave(SlaveNumber, CurrentSlaves) ->
    SlavePid = spawn_link(?MODULE, slave, [SlaveNumber]),
    dict:store(SlaveNumber, SlavePid, CurrentSlaves).

init_master(TotalSlaves) ->
    process_flag(trap_exit, true),
    Slaves = spawn_slaves(0, TotalSlaves, dict:new()),
    master(Slaves).

master(Slaves) ->
    receive
        { Message, Number } ->
            case dict:find(Number, Slaves) of
                { ok, Slave } ->
                    Slave ! { msg, Message };
                error ->
                    io:format("Slave ~p not found~n", [Number])
            end,
            master(Slaves);
        { 'EXIT', _SlavePid, SlaveNumber } ->
            io:format("master restarting dead slave~p~n", [SlaveNumber]),
            master(spawn_slave(SlaveNumber, Slaves));
        Other ->
            io:format("Debug: master ignored message ~p~n", [Other]),
            master(Slaves)
    end.

slave(MyNumber) ->
    receive
        { msg, die } ->
            exit(MyNumber);
        { msg, Message } ->
            io:format("Slave ~p got message ~p~n", [MyNumber, Message]),
            slave(MyNumber);
        Other ->
            io:format("Debug: slave ~p (~p) ignored message ~p~n", [MyNumber, self(), Other]),
            slave(MyNumber)
    end.
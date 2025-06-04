-module(server).
-export([start/1]).

start(ServerName) ->
    register(ServerName, spawn(fun() ->
        ets:new(sensor_data, [named_table, public, set]),
        loop()
                               end)).

loop() ->
    receive
        {From, {sensor_data, SensorName, Timestamp, Data}} ->
            Formatted = lists:flatten(format_time(Timestamp)),
            io:format("[~s] ~p reported: ~p~n", [Formatted, SensorName, Data]),
            ets:insert(sensor_data, {SensorName, {Timestamp, Data}}),
            loop();

        {From, stop} ->
            io:format("Received stop signal from ~p~n", [From]),
            From ! {self(), server_disconnect};

        Unknown ->
            io:format("Received unknown message: ~p~n", [Unknown]),
            loop()
    end.

format_time({{Y, M, D}, {H, Min, S}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Min, S]).

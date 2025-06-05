-module(server).
-export([start/1, loop/1]).

start(Name) ->
    register(Name, spawn(fun() ->
        net_kernel:monitor_nodes(true),
        loop(Name)
                         end)).

loop(Name) ->
    receive
        {From, {sensor_data, SensorName, Timestamp, Data}} ->
            io:format("~p: Received data from ~p at ~p: ~p~n", [Name, SensorName, Timestamp, Data]),
            loop(Name);

        {node_down, Node} ->
            io:format("~p: Detected node down: ~p~n", [Name, Node]),
            loop(Name);

        Other ->
            io:format("~p: Unexpected message: ~p~n", [Name, Other]),
            loop(Name)
    end.

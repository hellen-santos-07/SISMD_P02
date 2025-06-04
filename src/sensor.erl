-module(sensor).
-export([start/1, start/2, start/3, send_msg/1, stop_sensor/1, add_neighbors/2]).
-import(rand, [uniform/1]).

start(Name) ->
    start(Name, []).

start(Name, Neighbors) ->
    start(Name, Neighbors, false).

start(Name, Neighbors, UseRelay) ->
    register(Name, spawn(fun() ->
        MyNode = node(),
        [_, Host] = string:split(atom_to_list(MyNode), "@"),
        ServerNode = list_to_atom("server1@" ++ Host),
        net_adm:ping(ServerNode),
        loop(Name, ServerNode, Neighbors, UseRelay)
                         end)).

send_msg(SensorName) ->
    SensorName ! send.

stop_sensor(SensorName) ->
    SensorName ! stop.

add_neighbors(SensorName, NewNeighbors) ->
    SensorName ! {update_neighbors, NewNeighbors}.

loop(Name, ServerNode, Neighbors, UseRelay) ->
    receive
        send ->
            Msg = generate_data(Name),
            try_send(Name, ServerNode, Neighbors, Msg, UseRelay),
            loop(Name, ServerNode, Neighbors, UseRelay);

        {relay, From, Msg} ->
            io:format("~p: Forwarding data from ~p to server~n", [Name, From]),
            try_send(Name, ServerNode, Neighbors, Msg, UseRelay),
            loop(Name, ServerNode, Neighbors, UseRelay);

        {update_neighbors, NewNeighbors} ->
            io:format("~p: Neighbors updated to ~p~n", [Name, NewNeighbors]),
            loop(Name, ServerNode, NewNeighbors, UseRelay);

        stop ->
            io:format("~p: Sensor stopping~n", [Name])
    end.

try_send(Name, ServerNode, Neighbors, Msg, true) ->
    io:format("~p: [UseRelay=true] Skipping server. Trying neighbors...~n", [Name]),
    try_relay(Name, Msg, Neighbors);

try_send(Name, ServerNode, Neighbors, Msg, false) ->
    case catch {central, ServerNode} ! {self(), Msg} of
        {'EXIT', _} ->
            io:format("~p: Could not send to server. Trying neighbors...~n", [Name]),
            try_relay(Name, Msg, Neighbors);
        _ -> ok
    end.

try_relay(Name, Msg, []) ->
    io:format("No neighbors to relay the message.~n");

try_relay(Name, Msg, [{NeighborName, NeighborNode} | Rest]) ->
    io:format("~p: Trying neighbor ~p on node ~p...~n", [Name, NeighborName, NeighborNode]),
    case catch {NeighborName, NeighborNode} ! {relay, Name, Msg} of
        {'EXIT', _} ->
            io:format("~p: Failed to contact ~p. Trying next neighbor...~n", [Name, NeighborName]),
            try_relay(Name, Msg, Rest);
        _ ->
            ok
    end;

try_relay(Name, Msg, [Invalid | Rest]) ->
    io:format("~p: Invalid neighbor format: ~p~n", [Name, Invalid]),
    try_relay(Name, Msg, Rest).

generate_data(Name) ->
    Timestamp = calendar:local_time(),
    Data = #{temperature => rand:uniform(30), humidity => rand:uniform(100)},
    {sensor_data, Name, Timestamp, Data}.

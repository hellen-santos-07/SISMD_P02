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
        net_kernel:monitor_nodes(true),
        self() ! tick,
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
        tick ->
            send_msg(Name),
            erlang:send_after(10000, self(), tick),
            loop(Name, ServerNode, Neighbors, UseRelay);

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

        {node_down, DownSensor} ->
            Filtered = lists:filter(
                fun ({DownSensorName, _}) -> DownSensorName =/= DownSensor end,
                Neighbors),
            io:format("~p: Notified that ~p went down. Updated neighbors: ~p~n", [Name, DownSensor, Filtered]),
            loop(Name, ServerNode, Filtered, UseRelay);

        {nodedown, DownNode} ->
            io:format("~p: Detected node down: ~p~n", [Name, DownNode]),

            {DeadSensors, Remaining} = lists:partition(
                fun ({_, Node}) -> Node =:= DownNode end,
                Neighbors),

            lists:foreach(
                fun ({DeadSensorName, _}) ->
                    lists:foreach(
                        fun ({NName, NNode}) ->
                            case rpc:call(NNode, erlang, whereis, [NName]) of
                                undefined -> ok;
                                Pid -> Pid ! {node_down, DeadSensorName}
                            end
                        end, Remaining)
                end, DeadSensors),

            io:format("~p: Removed ~p. Remaining neighbors: ~p~n", [Name, DeadSensors, Remaining]),
            loop(Name, ServerNode, Remaining, UseRelay);

        stop ->
            io:format("~p: Sensor stopping~n", [Name])
    end.


try_send(Name, ServerNode, Neighbors, Msg, true) ->
    io:format("~p: [UseRelay=true] Skipping server. Trying neighbors!~n", [Name]),
    try_relay(Name, Msg, Neighbors);

try_send(Name, ServerNode, Neighbors, Msg, false) ->
    case catch {central, ServerNode} ! {self(), Msg} of
        {'EXIT', _} ->
            io:format("~p: Could not send to server. Trying neighbors!~n", [Name]),
            try_relay(Name, Msg, Neighbors);
        _ -> ok
    end.

try_relay(Name, Msg, []) ->
    io:format("~p: No more neighbours to try :( .~n", [Name]);

try_relay(Name, Msg, [{NeighborName, NeighborNode} | Rest]) ->
    io:format("~p: Trying neighbour ~p on node ~p!~n", [Name, NeighborName, NeighborNode]),
    case net_adm:ping(NeighborNode) of
        pang ->
            io:format("~p: Neighbor node ~p is unreachable. Trying the next one!~n", [Name, NeighborNode]),
            try_relay(Name, Msg, Rest);
        pong ->
            case rpc:call(NeighborNode, erlang, whereis, [NeighborName]) of
                undefined ->
                    io:format("~p: Neighbour process ~p not found on ~p. Trying the next one!~n", [Name, NeighborName, NeighborNode]),
                    try_relay(Name, Msg, Rest);
                Pid when is_pid(Pid) ->
                    Pid ! {relay, Name, Msg},
                    io:format("~p: Successfully relayed through ~p~n", [Name, NeighborName])
            end
    end;

try_relay(Name, Msg, [Invalid | Rest]) ->
    io:format("~p: Invalid neighbour format: ~p~n", [Name, Invalid]),
    try_relay(Name, Msg, Rest).

generate_data(Name) ->
    Timestamp = calendar:local_time(),
    Data = #{temperature => rand:uniform(30), humidity => rand:uniform(100)},
    {sensor_data, Name, Timestamp, Data}.

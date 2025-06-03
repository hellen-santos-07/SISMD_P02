-module(sensor).
-export([start/1, add_remote/1, send_msg/4, stop_sensor/1]).
-import(rand, [uniform/1]).

start(Sensor) ->
    register(Sensor, spawn(fun() ->
        MyNode = node(),
        [_, Host] = string:split(atom_to_list(MyNode), "@"),
        ServerNode = list_to_atom("server1@" ++ Host),
        add_remote(ServerNode),
        loop()
                           end)).

add_remote(RemoteMachine) ->
    net_adm:ping(RemoteMachine).

send_msg(Sensor, Server, RemoteMachine, Message) ->
    Sensor ! {send, Server, RemoteMachine, Message}.

stop_sensor(Sensor) ->
    Sensor ! {stop_sensor}.

generate_data() ->
    %% Random example: temperature (°C) and humidity (%)
    Temp = rand:uniform(15) + 15,  % 15–30°C
    Hum = rand:uniform(50) + 30,  % 30–80%
    #{temperature => Temp, humidity => Hum}.


loop() ->
    SensorName = self(),
    send_data(),
    receive
        {stop_sensor} ->
            io:format("Sensor exiting...~n")
    after 5000 ->  % wait 5 seconds, then send again
        loop()
    end.

send_data() ->
    MyNode = node(),
    [_, Host] = string:split(atom_to_list(MyNode), "@"),
    ServerNode = list_to_atom("server1@" ++ Host),
    Timestamp = calendar:local_time(),
    Data = generate_data(),
    {registered_name, SensorName} = process_info(self(), registered_name),
    Message = {sensor_data, SensorName, Timestamp, Data},
    {central, ServerNode} ! {self(), Message}.
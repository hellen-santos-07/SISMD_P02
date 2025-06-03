-module(server).
-export([start/1, print_data/0, print_data/1]).

start(Server) ->
    register(Server, spawn(fun() -> init_loop() end)).

init_loop() ->
    ets:new(sensor_data, [named_table, public, bag]),
    loop().

loop() ->
    receive
        {From, stop} ->
            io:format("Received from ~p message to stop!~n",[From]),
            From ! {self(), server_disconnect};

        {From, {sensor_data, SensorName, Timestamp, Data}} ->
            io:format("[~p] ~p reported: ~p~n", [Timestamp, SensorName, Data]),
            ets:insert(sensor_data, {SensorName, {Timestamp, Data}}),
            loop();

        {From, Msg} ->
            io:format("Received ~p: ~p~n",[From, Msg]),
            io:format("Sending reply...~n"),
            From ! {self(), happy_to_receive_your_message},
            loop()
    end.

print_data() ->
    All = ets:tab2list(sensor_data),
    lists:foreach(fun({Sensor, {Timestamp, Data}}) ->
        io:format("[~p] ~p => ~p~n", [Timestamp, Sensor, Data])
                  end, All).

print_data(SensorName) ->
    Entries = ets:lookup(sensor_data, SensorName),
    lists:foreach(fun({_, {Timestamp, Data}}) ->
        io:format("[~p] => ~p~n", [Timestamp, Data])
                  end, Entries).

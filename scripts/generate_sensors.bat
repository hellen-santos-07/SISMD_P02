@echo off
set /p COUNT=How many sensors to start?
erl -noshell -pa ebin -eval "lists:foreach(fun(C) -> io:format(\"~s~n\", [C]) end, start_sensors:commands(%COUNT%)), halt()." > scripts\sensor_launch_cmds.bat

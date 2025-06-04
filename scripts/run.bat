@echo off
cd /d "%~dp0.."

set SCRIPTS_DIR=%CD%\scripts

echo Compiling helper module
erl -noshell -eval "compile:file(\"src/compile_all.erl\", [{outdir, \"ebin\"}]), halt()."

echo Compiling all modules
erl -noshell -pa ebin -eval "compile_all:all(), halt()."

echo Starting server node
start cmd /k erl -pa ebin -sname server1 -setcookie mycookie -eval "server:start(central)."

timeout /t 2 >nul

echo Generating sensor launch commands
call "%SCRIPTS_DIR%\generate_sensors.bat"

echo Launching sensors
call "%SCRIPTS_DIR%\sensor_launch_cmds.bat"

echo Done.

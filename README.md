# SISMD ERLANG EXERCISE - TP02

## Student:
Hellen Santos - 1190007

## Structure:
* src/sensor.erl – Sensor node logic
* src/server.erl – Server
* src/compile_all.erl – Helper to compile all modules
* scripts/run.bat – Project geral launcher
* scripts/generate_sensors.bat – Script that asks user for number of sensors and generates a helper sensor launcher script
* scripts/sensor_launch_cmds.bat – Auto-generated file that starts each sensor

## Conventions:
* Sensors generates data every 10 seconds, and send them to the server.
* Data is stored and retried if no route to the server is found.
* Neighbour information can be updated at runtime using:

## Failure Simulation:
* Closing a sensor terminal so it simulates a crash.
* Observing neighbor notifications.
* Ensuring messages still reach the server if alternate neighbors are available

## Running the Project:
1. cd SISMD_P02
2. .\scripts\run.bat
3. input the number of sensors you want to generate

Manually adding a new sensor:

``` erl -pa ebin -sname sensorN -setcookie bolacha -eval "sensor:start(sensorN, [{sensor1, 'sensor1@<HOST>'}], true)." ```



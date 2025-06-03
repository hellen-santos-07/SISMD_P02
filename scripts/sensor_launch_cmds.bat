start cmd /k "erl -pa ebin -sname sensor1 -setcookie mycookie -eval sensor:start('sensor1')."
start cmd /k "erl -pa ebin -sname sensor2 -setcookie mycookie -eval sensor:start('sensor2')."

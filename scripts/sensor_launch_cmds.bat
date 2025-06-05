start cmd /k erl -pa ebin -sname sensor1 -setcookie bolacha -eval "sensor:start('sensor1', [], false)."
start cmd /k erl -pa ebin -sname sensor2 -setcookie bolacha -eval "sensor:start('sensor2', [], false)."
start cmd /k erl -pa ebin -sname sensor3 -setcookie bolacha -eval "sensor:start('sensor3', [{sensor1,'sensor1@DESKTOP-MDQH73R'},{sensor2,'sensor2@DESKTOP-MDQH73R'}], true)."
start cmd /k erl -pa ebin -sname sensor4 -setcookie bolacha -eval "sensor:start('sensor4', [{sensor1,'sensor1@DESKTOP-MDQH73R'},{sensor2,'sensor2@DESKTOP-MDQH73R'}], true)."

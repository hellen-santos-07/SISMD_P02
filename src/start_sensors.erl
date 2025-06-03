%%%-------------------------------------------------------------------
%%% @author Hellen
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. jun. 2025 12:09
%%%-------------------------------------------------------------------
-module(start_sensors).
%% API
-export([commands/1]).

commands(N) when is_integer(N), N > 0 ->
    [ io_lib:format(
        "start cmd /k \"erl -pa ebin -sname sensor~p -setcookie mycookie -eval sensor:start('sensor~p').\"", 
        [Idx, Idx]
      )
    || Idx <- lists:seq(1, N) ].
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
  {ok, Hostname} = inet:gethostname(),
  Host = string:uppercase(Hostname),  lists:map(fun(Idx) ->
    Name = "sensor" ++ integer_to_list(Idx),
    UseRelay = Idx > 2,
    NeighborStr =
      case Idx of
        1 -> "[]";
        2 -> "[]";
        _ ->
          io_lib:format(
            "[{sensor1,'sensor1@~s'},{sensor2,'sensor2@~s'}]",
            [Host, Host]
          )
      end,

    EvalStr = io_lib:format(
      "sensor:start('~s', ~s, ~p).",
      [Name, NeighborStr, UseRelay]
    ),

    FullCmd = io_lib:format(
      "start cmd /k erl -pa ebin -sname ~s -setcookie mycookie -eval \"~s\"",
      [Name, lists:flatten(EvalStr)]
    ),
    lists:flatten(FullCmd)
            end, lists:seq(1, N)).
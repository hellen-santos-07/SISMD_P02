%%%-------------------------------------------------------------------
%%% @author Hellen
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. jun. 2025 10:23
%%%-------------------------------------------------------------------
-module(compile_all).
%% API
-export([all/0]).

all() ->
    compile:file("src/server.erl", [{outdir, "ebin"}]),
    compile:file("src/sensor.erl", [{outdir, "ebin"}]),
    compile:file("src/start_sensors.erl", [{outdir, "ebin"}]).
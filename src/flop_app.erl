-module(flop_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

start_result({ok, Pid}) ->
    {ok, Pid};
start_result(Other) ->
    {error, Other}.

start(_Type, _Args) ->
    start_result(flop_sup:start_link()).

stop(_State) ->
    ok.

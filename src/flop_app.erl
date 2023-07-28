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

start(Type, Args) ->
    start_result(flop_sup:start_link()).

stop(State) ->
    ok.

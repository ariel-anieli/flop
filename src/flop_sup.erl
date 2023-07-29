-module(flop_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Strategy = #{
		 strategy  => one_for_one
		},
    Servers  = #{
		 id    => flop,
		 start => {flop, start_link, []},
		 type  => worker
		},
    Children = [Servers],
    {ok, {Strategy, Children}}.

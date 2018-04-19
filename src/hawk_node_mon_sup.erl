-module(hawk_node_mon_sup).

-behaviour(supervisor).
-export([init/1]).

% API
-export([
    start_link/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    Child1 =
        {hawk_node_mon,
            {hawk_node_mon, start_link, []},
            permanent, 5000, worker,
            [hawk_node_mon]},

    Children = [Child1],
    RestartStrategy = {one_for_one, 500, 10},
    {ok, {RestartStrategy, Children}}.
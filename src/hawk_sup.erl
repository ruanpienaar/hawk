-module(hawk_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0
]).

start_link() ->
    supervisor:start_link(?MODULE, {}).

init({}) ->
    {
        ok,
        {
            #{
                strategy => one_for_all,
                intensity => 100,
                period => 5
            },
            [
                #{
                    id => hawk_nodes_sup,
                    start => {hawk_nodes_sup, start_link, []},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [hawk_nodes_sup]
                },
                #{
                    id => hawk_node_mon_sup,
                    start => {hawk_node_mon_sup, start_link, []},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [hawk_node_mon_sup]
                }
            ]
        }
    }.
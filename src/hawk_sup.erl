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
        #{
            strategy => one_for_all,
            intensity => 100,
            period => timer:seconds(5),
            auto_shutdown => all_significant
        },
        [
            #{
                id => hawk_nodes_sup,
                start => {hawk_nodes_sup, start_link, []},
                restart => permanent,
                significant => true,
                shutdown => infinity,
                type => supervisor,
                modules => [hawk_nodes_sup]
            },
            #{
                id => hawk_node_mon_sup,
                start => {hawk_node_mon_sup, start_link, []},
                restart => permanent,
                significant => true,
                shutdown => infinity,
                type => supervisor,
                modules => [hawk_node_mon_sup]
            }
        ]
    }.
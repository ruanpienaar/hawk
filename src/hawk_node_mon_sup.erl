-module(hawk_node_mon_sup).

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
            restart => one_for_one,
            intensity => 500,
            period => timer:seconds(5)
        },
        [
            #{
                id => hawk_node_mon,
                start => {hawk_node_mon, start_link, []},
                restart => permanent,
                significant => false,
                shutdown => timer:seconds(60),
                type => worker,
                modules => [hawk_node_mon]
            }
        ]
    }.
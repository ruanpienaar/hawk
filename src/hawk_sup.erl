-module(hawk_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

%% TODO: add maps...
    % #{id := Id,
    %   start := {Mod, start_link, Args},
    %   restart => permanent,
    %   shutdown => 5000,
    %   type => Type,
    %   modules => [Mod]}.
-define(CHILD(Mod),
    {Mod, {Mod, start_link, []}, permanent, 5000, supervisor, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 500, 10}, [
        ?CHILD(hawk_nodes_sup),
        ?CHILD(hawk_node_mon_sup)
    ]} }.
-module(hawk_nodes_sup).

-behaviour(supervisor).
-export([init/1]).

%% API
-export([
    start_link/0,
    start_child/4,
    delete_child/1,
    id/1
]).

%% NB: not working with 17... removed
%-define(CHILD(Id, Mod, Type, Args),
%    #{id       => Id,                      % mandatory
%      start    => {Mod, start_link, Args}, % mandatory
%      restart  => permanent,               % optional
%      shutdown => 5000,                    % optional
%      type     => Type,                    % optional
%      modules  => [Mod]                    % optional
%    }
%).
-define(CHILD(Id, Mod, Type, Args),
	{Id, {Mod, start_link, Args}, transient, 5000, Type, [Mod]}
).

-type start_child_return() :: {'error', term()} |
                              {'ok','undefined' | pid()}.
-type delete_child_return() :: ok | {error, no_such_node}.
-export_types([
    start_child_return/0,
    delete_child_return/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 500, 10}, []}}.

-spec start_child(atom(), atom(), hawk:callbacks(), hawk:callbacks())
    -> start_child_return().
start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback)
    when is_atom(Node),
         is_atom(Cookie),
         is_list(ConnectedCallback),
         is_list(DisconnectedCallback) ->
    supervisor:start_child(
        ?MODULE,
        ?CHILD(id(Node), hawk_node, worker, [Node, Cookie, ConnectedCallback, DisconnectedCallback])
    ).

-spec delete_child(atom()) -> delete_child_return().
delete_child(Node) when is_atom(Node) ->
    NodeId = id(Node),
    case supervisor:terminate_child(?MODULE, NodeId) of
        {error,not_found} ->
            {error, no_such_node};
        ok ->
            ok = supervisor:delete_child(?MODULE, NodeId)
    end.

id(Node) ->
    list_to_atom("hawk_node_"++atom_to_list(Node)).
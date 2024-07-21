-module(hawk_nodes_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0,
    start_child/4,
    delete_child/1,
    id/1,
    children/0,
    nodes/0
]).

-type start_child_return() :: {'error', term()} |
                              {'ok','undefined' | pid()}.
-type delete_child_return() :: ok | {error, no_such_node}.
-export_types([
    start_child_return/0,
    delete_child_return/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    _ = ets:new(hawk_nodes, [named_table, set, public]),
    {
        ok,
        {
            #{
                restart => one_for_one,
                intensity => 500,
                period => 5
            },
            []
        }
    }.

-spec start_child(atom(), atom(), hawk:callbacks(), hawk:callbacks())
    -> start_child_return().
start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback)
    when is_atom(Node),
         is_atom(Cookie),
         is_list(ConnectedCallback),
         is_list(DisconnectedCallback) ->
    case do_start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback) of
        {ok, _pid} ->
            {ok, _pid};
        {ok, _pid, _info} ->
            {ok, _pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

do_start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    supervisor:start_child(
        ?MODULE,
        #{
            id => id(Node),
            start => {
                hawk_node2,
                start_link,
                [Node, Cookie, ConnectedCallback, DisconnectedCallback]
            },
            restart => permanent,
            significant => false,
            shutdown => timer:seconds(60),
            type => worker,
            modules => [hawk_node2]
        }
    ).

-spec delete_child(atom()) -> delete_child_return().
delete_child(Node) when is_atom(Node) ->
    NodeId = id(Node),
    Ret =
        case supervisor:terminate_child(?MODULE, NodeId) of
            {error, not_found} ->
                {error, no_such_node};
            ok ->
                ok = supervisor:delete_child(?MODULE, NodeId)
        end,
    _ = erlang:disconnect_node(Node),
    true = ets:delete(hawk_nodes, Node),
    Ret.

id(Node) ->
    list_to_atom(atom_to_list(Node)).

children() ->
    supervisor:which_children(?MODULE).

nodes() ->
    ets:tab2list(hawk_nodes).
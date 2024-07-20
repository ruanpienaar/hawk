-module(hawk_nodes_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0,
    start_child/4,
    delete_child/1,
    id/1
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
    {
        ok,
        #{
            restart => one_for_one,
            intensity => 500,
            period => timer:seconds(5)
        },
        []
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
        {error, already_started} ->
            {ok, whereis(id(Node))}
    end.

do_start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    supervisor:start_child(
        ?MODULE,
        #{
            id => id(Node),
            start => {hawk_node, start_link, [Node, Cookie, ConnectedCallback, DisconnectedCallback]},
            restart => transient,
            significant => false,
            shutdown => timer:seconds(60),
            type => worker,
            modules => [hawk_node]
        }
    ).

-spec delete_child(atom()) -> delete_child_return().
delete_child(Node) when is_atom(Node) ->
    NodeId = id(Node),
    case supervisor:terminate_child(?MODULE, NodeId) of
        {error, not_found} ->
            {error, no_such_node};
        ok ->
            ok = supervisor:delete_child(?MODULE, NodeId)
    end.

id(Node) ->
    list_to_atom("hawk_node_"++atom_to_list(Node)).
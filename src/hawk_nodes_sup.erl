-module(hawk_nodes_sup).
-behaviour(supervisor).

-define(CHILD(Id, Mod, Type, Args),
    #{id       => Id,                      % mandatory
      start    => {Mod, start_link, Args}, % mandatory
      restart  => permanent,               % optional
      shutdown => 5000,                    % optional
      type     => Type,                    % optional
      modules  => [Mod]                    % optional
    }
).

%% API
-export([
    init/1,
    start_link/0,
    start_child/4,
    delete_child/1,
    id/1
]).

-type start_child_return() :: {'error', term()} |
                              {'ok','undefined' | pid()} |
                              {'ok','undefined' | pid(), term()}.

-type delete_child_return() :: ok | {error, no_such_node}.

-export_types([
    start_child_return/0,
    delete_child_return/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    RestartStrategy = {one_for_one, 500, 10},
    {ok, {RestartStrategy, []}}.

-spec start_child(atom(), atom(), hawk:callbacks(), hawk:callbacks())
    -> start_child_return().
start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback)
    when is_atom(Node),
         is_atom(Cookie),
         is_list(ConnectedCallback),
         is_list(DisconnectedCallback) ->
    supervisor:start_child(?MODULE,
        ?CHILD(Node, hawk_node, worker, [Node, Cookie, ConnectedCallback, DisconnectedCallback])
    ).

-spec delete_child(atom()) -> delete_child_return().
delete_child(Node) when is_atom(Node) ->
    case whereis(id(Node)) of
        undefined ->
            {error, no_such_node};
        Pid when is_pid(Pid) ->
            ok = supervisor:terminate_child(?MODULE, Node),
            ok = supervisor:delete_child(?MODULE, Node)
    end.

id(Node) ->
    list_to_atom("hawk_node_"++atom_to_list(Node)).
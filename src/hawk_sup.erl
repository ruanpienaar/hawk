-module(hawk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/4,
         delete_child/1
]).

-type start_child_return() ::
    {ok, pid()} |
    {error, already_present} |
    {error, {already_started, pid()}}.

-type delete_child_return() ::
    ok | {error, no_such_node}.

-export_types([
    start_child_return/0,
    delete_child_return/0
]).

%% the supervised process' id is Node.

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args),
    {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).
%% TODO: add maps...
    % #{id := Id,
    %   start := {Mod, start_link, Args},
    %   restart => permanent,
    %   shutdown => 5000,
    %   type => Type,
    %   modules => [Mod]}.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(), atom(), list(), list())
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
    case whereis(Node) of
        undefined ->
            {error, no_such_node};
        Pid when is_pid(Pid) ->
            ok = supervisor:terminate_child(?MODULE, Node),
            ok = supervisor:delete_child(?MODULE, Node)
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
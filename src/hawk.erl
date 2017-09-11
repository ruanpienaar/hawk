-module(hawk).

-compile({no_auto_import,[nodes/0]}).

-export([
    start/0,
    stop/0
]).
-export([
    nodes/0,
    node_exists/1,
    add_node/2, add_node/4,
    add_connect_callback/2,
    add_disconnect_callback/2,
    remove_connect_callback/2,
    remove_disconnect_callback/2,
    remove_node/1,
    update_cookie/2,
    node_state/1,
    connected_nodes/0
]).

%% As guidance template functions
-export([
    connected/2,
    disconnected/1
]).

-define(R, hawk_req).

-type hawk_node_call_return() :: timeout | {error, connecting} | {ok, term()}.
-type callback_names_return() :: {ok, pid(), list()} | hawk_node_call_return().
-type hawk_node_cmd() ::
    state |
    {add_connect_callback, {Name :: term(),ConnectCallback :: list()}} |
    {add_disconnect_callback, {Name :: term(),DisconnectCallback :: list()}} |
    {remove_connect_callback, Name :: term()} |
    {remove_disconnect_callback, Name :: term()} |
    callbacks.

start() ->
    application:ensure_all_started(hawk).

stop() ->
    application:stop(hawk).

-spec nodes() -> list().
nodes() ->
    [ N || {N,_,worker,[hawk_node]} <- supervisor:which_children(hawk_nodes_sup) ].

-spec node_exists(atom()) -> false | callback_names_return().
node_exists(Node) ->
    case whereis(Node) of
        undefined ->
            false;
        Pid when is_pid(Pid) ->
            callback_names(Pid, Node)
    end.

-spec add_node(atom(), atom())
    -> hawk_nodes_sup:start_child_return() |
       {error,{already_started,pid()}}.
add_node(Node, Cookie) ->
    case node_exists(Node) of
        false ->
            add_node(Node, Cookie, [], []);
        {ok, Pid, _Callbacks} ->
            {error,{already_started,Pid}}
    end.

-spec add_node(atom(), atom(), list(), list())
    -> hawk_nodes_sup:start_child_return().
%% TODO: double check the format of the callback
add_node(Node, Cookie, ConnectedCallback, DisconnectedCallback)
        when is_atom(Node), is_atom(Cookie), is_list(ConnectedCallback), is_list(DisconnectedCallback) ->
    case node_exists(Node) of
        false ->
            hawk_nodes_sup:start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback);
        {ok, Pid, _Callbacks} ->
            ok = lists:foreach(fun({Name,ConnectCallback}) ->
                %% hawk_node handles the dups
                add_connect_callback(Node, {Name,ConnectCallback})
            end, ConnectedCallback),
            ok = lists:foreach(fun({Name, DisconnectCallback}) ->
                %% hawk_node handles the dups
                add_disconnect_callback(Node, {Name, DisconnectCallback})
            end, DisconnectedCallback),
            {error,{already_started,Pid}}
    end.

-spec add_connect_callback(atom(), {term(), fun()})
    -> hawk_node_call_return().
add_connect_callback(Node, {Name,ConnectCallback}) when is_function(ConnectCallback) ->
    call(Node, {add_connect_callback, {Name, ConnectCallback}}).

-spec add_disconnect_callback(atom(), {term(), fun()})
    -> hawk_node_call_return().
add_disconnect_callback(Node, {Name, DisconnectCallback}) when is_function(DisconnectCallback) ->
    call(Node, {add_disconnect_callback, {Name, DisconnectCallback}}).

-spec remove_connect_callback(atom(), term())
    -> hawk_node_call_return().
remove_connect_callback(Node, Name) ->
    call(Node, {remove_connect_callback, Name}).

-spec remove_disconnect_callback(atom(), term())
    -> hawk_node_call_return().
remove_disconnect_callback(Node, Name) ->
    call(Node, {remove_disconnect_callback, Name}).

-spec remove_node(atom()) -> hawk_nodes_sup:delete_child_return().
remove_node(Node) ->
    hawk_nodes_sup:delete_child(Node).

update_cookie(_Node, _NewCookie) ->
    ok.

-spec node_state(atom()) -> hawk_node_call_return().
node_state(Node) ->
    call(Node, state).

-spec callback_names(pid(), atom())
    -> {ok, pid(), list()} | hawk_node_call_return().
callback_names(Pid, Node) ->
    case call(Node, callbacks) of
        {ok, {ConCBS, DisCBS}} when is_list(ConCBS), is_list(DisCBS) ->
            Callbacknames = lists:map(fun({Name,_}) -> Name end, lists:flatten([ConCBS,DisCBS])),
            {ok, Pid, Callbacknames};
        Else ->
            Else
    end.

connected_nodes() ->
    lists:filter(fun(Node) ->
        node_state(Node) =/= {error,connecting}
    end, nodes()).

%%-------------------------------------------------------------------------------------------

-spec call(atom(), hawk_node_cmd()) -> hawk_node_call_return().
call(Node, Cmd) ->
    call(Node, Cmd, 1000).

-spec call(atom(), hawk_node_cmd(), non_neg_integer())
    -> hawk_node_call_return().
call(Node, Cmd, Timeout) ->
    whereis(Node) ! {call, Cmd, self()},
    receive
        {response, connecting} ->
            {error, connecting};
        {response, Response} ->
            {ok, Response}
    after
        Timeout ->
            timeout
    end.
%% -----------------
%% Helper callbacks:

-spec connected(atom(), atom()) -> ok.
connected(Node, Cookie) ->
    fun() -> error_logger:info_msg("Connected!!! ~p ~p ~n", [Node, Cookie]) end.

-spec disconnected(atom()) -> ok.
disconnected(Node) ->
    fun() -> error_logger:info_msg("disConnected!!! ~p ~n", [Node]) end.

%% -----------------
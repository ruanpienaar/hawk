-module(hawk).

% -compile(export_all).
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
    node_state/1
]).
-define(R, hawk_req).

%% ------------
start() -> [ ok = application:ensure_started(APP) || APP <- apps() ].
stop() -> [ ok = application:ensure_started(APP) || APP <- lists:reverse(apps()) ].
apps() ->
    [hawk].
%% ------------

nodes() ->
    [ N || {N,_,worker,[hawk_node]} <- supervisor:which_children(hawk_sup) ].

node_exists(Node) ->
    case whereis(Node) of
        undefined ->
            false;
        Pid when is_pid(Pid) ->
            callback_names(Pid, Node)
    end.

add_node(Node, Cookie) ->
    hawk_sup:start_child(Node, Cookie, [], []).

add_node(Node, Cookie, ConnectedCallback, DisconnectedCallback)
        when is_atom(Node), is_atom(Cookie), is_list(ConnectedCallback), is_list(DisconnectedCallback) ->
    hawk_sup:start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback).

add_connect_callback(Node, {Name,ConnectCallback}) when is_function(ConnectCallback) ->
    call(Node, {add_connect_callback, {Name, ConnectCallback}}).

add_disconnect_callback(Node, {Name, DisconnectCallback}) when is_function(DisconnectCallback) ->
    call(Node, {add_disconnect_callback, {Name, DisconnectCallback}}).

remove_connect_callback(Node, Name) ->
    call(Node, {remove_connect_callback, Name}).

remove_disconnect_callback(Node, Name) ->
    call(Node, {remove_disconnect_callback, Name}).

remove_node(Node) ->
    hawk_sup:delete_child(Node).

update_cookie(_Node, _NewCookie) ->
    ok.

node_state(Node) ->
    call(Node, state).

callback_names(Pid, Node) ->
    case call(Node, callbacks) of
        {ok, {ConCBS, DisCBS}} when is_list(ConCBS), is_list(DisCBS) ->
            Callbacknames = lists:map(fun({Name,_}) -> Name end, lists:flatten([ConCBS,DisCBS])),
            {ok, Pid, Callbacknames};
        Else ->
            Else
    end.

call(Node, Cmd) ->
    call(Node, Cmd, 5000).

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

-spec connected(node(), atom()) -> ok.
connected(Node, Cookie) ->
    io:format("Connected!!! ~p ~p ~n", [Node, Cookie]).

-spec disconnected(node()) -> ok.
disconnected(Node) ->
    io:format("disConnected!!! ~p ~n", [Node]).

%% -----------------
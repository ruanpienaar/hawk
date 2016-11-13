-module(hawk).

-compile(export_all).
% -export([
% ]).
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
            Callbacknames = callback_names(Node),
            {ok, Pid, Callbacknames}
    end.

add_node(Node, Cookie) ->
    add_node(Node, Cookie,
        [{hawk_default_connected_callback, fun() -> connected(Node, Cookie) end}],
        [{hawk_default_disconnect_callback, fun() -> disconnected(Node) end}]).

add_node(Node, Cookie, ConnectedCallback, DisconnectedCallback)
        when is_atom(Node), is_atom(Cookie), is_list(ConnectedCallback), is_list(DisconnectedCallback) ->
    hawk_sup:start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback).

add_connect_callback(Node, ConnectCallback) when is_function(ConnectCallback) ->
    call(Node, {add_connect_callback, ConnectCallback}).

add_disconnect_callback(Node, DisconnectCallback) when is_function(DisconnectCallback) ->
    call(Node, {add_disconnect_callback, DisconnectCallback}).

remove_node(Node) ->
    hawk_sup:delete_child(Node).

update_cookie(_Node, _NewCookie) ->
    ok.

restart(Node) ->
    call(Node, restart).

node_state(Node) ->
    call(Node, state).

callback_names(Node) ->
    case call(Node, callbacks) of
        {ok, ConCBS, DisCBS} when is_list(ConCBS), is_list(DisCBS) ->
            lists:map(fun({Name,_}) -> Name end, [ConCBS|DisCBS])
    end.

call(Node, Cmd) ->
    call(Node, Cmd, 5000).

call(Node, Cmd, Timeout) ->
    whereis(Node) ! {call, Cmd, self()},
    receive
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
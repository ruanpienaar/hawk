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

add_node(Node, Cookie) ->
    add_node(Node, Cookie, fun connected/1, fun disconnected/1).

add_node(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    hawk_sup:start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback).

remove_node(Node) ->
    hawk_sup:delete_child(Node).

update_cookie(Node, NewCookie) ->
    ok.

restart(Node) ->
    call(Node, restart).

node_state(Node) ->
    call(Node, state).

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

-spec connected(node()) -> ok.
connected(Node, Cookie) ->
    io:format("Connected!!! ~p ~p ~n", [Node, Cookie]).

-spec disconnected(node()) -> ok.
disconnected(Node) ->
    io:format("disConnected!!! ~p ~n", [Node]).

%% -----------------
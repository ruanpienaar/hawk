-module(hawk_node2).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

-export([
    start_link/4,
    init/1,
    callback_mode/0,
    handle_event/4
]).

-spec start_link(node(), atom(), list(), list()) -> {ok, pid()}.
start_link(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks) ->
    gen_statem:start_link(
        {local, hawk_nodes_sup:id(Node)},
        {Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks},
        []
    ).

callback_mode() ->
    [handle_event_function, state_enter].

init({Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks}) ->
    ok = hawk_node_mon:add_node(hawk_nodes_sup:id(Node), self()),
    {
        ok,
        case lists:member(Node, nodes() ++ nodes(hidden)) of
            true ->
                connected;
            false ->
                disconnected
        end,
        initial_data(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks)
    }.

handle_event(enter, _, disconnected, _Data) ->
    keep_state_and_data;
handle_event(enter, _, connected, _Data) ->
    keep_state_and_data.

%% Still using lists for callbacks, so we can call them in order
initial_data(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks) ->
    #{
        connected => undefined,
        node => Node,
        cookie => Cookie,
        conn_cb_list => ConnectedCallbacks,
        disc_cb_list => DisconnectedCallbacks,
        connection_retries => hawk_config:connection_retries(),
        conn_retry_wait => hawk_config:conn_retry_wait()
    }.
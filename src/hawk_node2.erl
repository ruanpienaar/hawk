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
        ?MODULE,
        {Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks},
        []
    ).

% TODO: use sys get state
% node_state(Node) ->
%     ok.

callback_mode() ->
    [handle_event_function, state_enter].

init({Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks}) ->
    ?LOG_NOTICE(#{
        module => ?MODULE,
        function => ?FUNCTION_NAME,
        arity => ?FUNCTION_ARITY,
        node => Node,
        cookie => Cookie
    }),
    ok = hawk_node_mon:add_node(hawk_nodes_sup:id(Node), self()),
    {
        ok,
        % case lists:member(Node, nodes() ++ nodes(hidden)) of
        %     true ->
        %         connected;
        %     false ->
        %         disconnected
        % end,
        disconnected,
        initial_data(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks)
    }.

handle_event(info, {call, state, FromPid}, _State, Data) ->
    FromPid ! {response, Data},
    keep_state_and_data;
handle_event(info, {call, callbacks, FromPid}, _State, #{conn_cb_list := CCBL, disc_cb_list := DCBL } = _Data) ->
    FromPid ! {response, {CCBL, DCBL}},
    keep_state_and_data;
handle_event(enter, _, disconnected, #{node := Node, cookie := Cookie} = Data) ->
    true = erlang:set_cookie(Node, Cookie),
    ?LOG_NOTICE(#{
        module => ?MODULE,
        function => ?FUNCTION_NAME,
        arity => ?FUNCTION_ARITY,
        node_pre_connect => Node
    }),
    Ans = net_kernel:connect_node(Node),
    ?LOG_NOTICE(#{
        module => ?MODULE,
        function => ?FUNCTION_NAME,
        arity => ?FUNCTION_ARITY,
        node_post_connect => Node
    }),
    case Ans of
        true ->
            {keep_state, Data#{connected => true}};
        false ->
            {keep_state, Data#{connected => false}, [{state_timeout, 100, reconnect}]}
    end;
handle_event(info, {nodeup, Node}, disconnected, #{ node := Node } = Data) ->
    ?LOG_NOTICE(#{
        module => ?MODULE,
        function => ?FUNCTION_NAME,
        arity => ?FUNCTION_ARITY,
        data => Data
    }),
    {next_state, connected, Data#{connected => true}};
handle_event(state_timeout, reconnect, disconnected, Data) ->
    ?LOG_NOTICE(#{
        module => ?MODULE,
        function => ?FUNCTION_NAME,
        arity => ?FUNCTION_ARITY,
        data => Data,
        event_context => reconnect
    }),
    {next_state, disconnected, Data};
handle_event(enter, _, connected, Data) ->
    ?LOG_NOTICE(#{
        module => ?MODULE,
        function => ?FUNCTION_NAME,
        arity => ?FUNCTION_ARITY,
        event_context => enter,
        state => connected
    }),
    {keep_state, Data#{connected => true}}.

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
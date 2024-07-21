-module(hawk_node2).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

-export([
    start_link/4,
    init/1,
    callback_mode/0,
    handle_event/4,
    code_change/4,
    terminate/3,

    % just for tracing
    going_to_remove_node/1,
    end_of_test/1
]).

-spec start_link(node(), atom(), list(), list()) -> {ok, pid()}.
start_link(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks) ->
    gen_statem:start_link(
        {local, hawk_nodes_sup:id(Node)},
        ?MODULE,
        {Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks},
        []
    ).

going_to_remove_node(_Node) ->
    ok.

end_of_test(_Node) ->
    ok.

% TODO: use sys get state
% node_state(Node) ->
%     ok.

callback_mode() ->
    [handle_event_function, state_enter].

init({Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks}) ->
    true = ets:insert(hawk_nodes, {Node, false}),
    Data = initial_data(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks),
    ?LOG_NOTICE(#{data => Data}),
    {ok, disconnected, Data}.

handle_event(
        info,
        {call, state, FromPid},
        _State,
        Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    FromPid ! {response, Data},
    keep_state_and_data;
handle_event(
        info,
        {call, callbacks, FromPid},
        _State,
        #{ conn_cb_list := CCBL, disc_cb_list := DCBL } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    FromPid ! {response, {CCBL, DCBL}},
    keep_state_and_data;
handle_event(
        enter,
        _,
        disconnected,
        #{ node := Node } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    true = ets:insert(hawk_nodes, {Node, false}),
    {keep_state, Data, [{state_timeout, 0, reconnect}]};
handle_event(
        enter,
        _,
        connected,
        #{ node := Node } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    true = ets:insert(hawk_nodes, {Node, true}),
    {keep_state, Data#{connected => true}};
handle_event(
        state_timeout,
        {nodeup, Node},
        disconnected,
        #{ node := Node } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    {next_state, connected, Data#{connected => true}};
handle_event(
        info,
        {nodeup, Node},
        connected,
        #{ node := Node } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    keep_state_and_data;
handle_event(
        info,
        {nodeup, Node},
        disconnected,
        #{ node := Node } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    true = ets:insert(hawk_nodes, {Node, true}),
    {next_state, connected, Data#{connected => true}};
handle_event(
        info,
        {nodedown, Node},
        connected,
        #{ node := Node } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    {next_state, disconnected, Data#{connected => false}};
handle_event(
        info,
        {nodedown, Node},
        disconnected,
        #{ node := Node } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    {next_state, disconnected, Data#{connected => false}};
handle_event(
        state_timeout,
        reconnect,
        disconnected,
        #{
            node := Node,
            cookie := Cookie,
            connection_retries := Retries,
            backoff_type := BackoffType,
            backoff_wait := BackoffWait
        } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    true = erlang:set_cookie(Node, Cookie),
    case net_kernel:connect_node(Node) of
        true ->
            ok = hawk_node_mon2:add_node(hawk_nodes_sup:id(Node), self()),
            Data2 = Data#{
                backoff_type => hawk_config:backoff_type(),
                backoff_wait => hawk_config:backoff_wait(),
                connected => true
            },
            {next_state, connected, Data2};
        false ->
            NextBackoffWait = next_backoff(BackoffType, BackoffWait),
            Data2 = Data#{
                connection_retries => Retries - 1,
                connected => false,
                backoff_wait => NextBackoffWait
            },
            {keep_state, Data2, [{state_timeout, NextBackoffWait, reconnect}]}
    end.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(Reason, State, Data) ->
    ?LOG_WARNING(#{
        terminate_reason => Reason,
        terminate_state => State,
        terminate_data => Data
    }).

%% Still using lists for callbacks, so we can call them in order
initial_data(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks) ->
    #{
        connected => undefined,
        node => Node,
        cookie => Cookie,
        conn_cb_list => ConnectedCallbacks,
        disc_cb_list => DisconnectedCallbacks,
        connection_retries => hawk_config:connection_retries(),
        backoff_type => hawk_config:backoff_type(),
        backoff_wait => hawk_config:backoff_wait()
    }.

next_backoff(fixed, BackoffWait) ->
    BackoffWait;
next_backoff({exponential, Times, MaxTimes}, BackoffWait) when Times >= MaxTimes ->
    BackoffWait;
next_backoff({exponential, Times, MaxTimes}, BackoffWait) when Times < MaxTimes ->
    BackoffWait bsl 1.
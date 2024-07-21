-module(hawk_node2).

% TODO: implement max connection retries.

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

-export([
    start_link/4,
    callback_names/1,
    add_connect_callback/2,
    add_disconnect_callback/2,
    remove_connect_callback/2,
    remove_disconnect_callback/2,
    is_node_started/1,
    init/1,
    callback_mode/0,
    handle_event/4,
    code_change/4,
    terminate/3
]).

-spec start_link(node(), atom(), list(), list()) -> {ok, pid()}.
start_link(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks) ->
    gen_statem:start_link(
        {local, hawk_nodes_sup:id(Node)},
        ?MODULE,
        {Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks},
        []
    ).

callback_names(Node) ->
    gen_statem:call(hawk_nodes_sup:id(Node), callbacks).

add_connect_callback(Node, {Name, ConnectCallback}) ->
    gen_statem:call(hawk_nodes_sup:id(Node), {add_connect_callback, {Name, ConnectCallback}}).

add_disconnect_callback(Node, {Name, DisconnectCallback}) ->
    gen_statem:call(hawk_ndoes_sup:id(Node), {add_disconnect_callback, {Name, DisconnectCallback}}).

remove_connect_callback(Node, Name) ->
    gen_statem:call(hawk_ndoes_sup:id(Node), {remove_connect_callback, Name}).

remove_disconnect_callback(Node, Name) ->
    gen_statem:call(hawk_ndoes_sup:id(Node), {remove_disconnect_callback, Name}).

is_node_started(Node) ->
    case whereis(hawk_nodes_sup:id(Node)) of
        undefined ->
            false;
        Pid when is_pid(Pid) ->
            true
    end.

callback_mode() ->
    [handle_event_function, state_enter].

init({Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks}) ->
    _ = process_flag(trap_exit, true),
    ok = hawk_node_mon2:add_node(hawk_nodes_sup:id(Node), self()),
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
        {call, From},
        callbacks,
        _State,
        #{ conn_cb_list := CCBL, disc_cb_list := DCBL } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    {keep_state_and_data, [{reply, From, {ok, {CCBL, DCBL}}}]};
handle_event(
        {call, From},
        {add_connect_callback, {Name, ConnectCallback}},
        _State,
        #{ conn_cb_list := CCBL } = Data
    ) ->
    Data2 = Data#{ conn_cb_list => lists:append(CCBL, [{Name, ConnectCallback}]) },
    {keep_state, Data2, [{reply, From, ok}]};
handle_event(
        {call, From},
        {add_disconnect_callback, {Name, DisconnectCallback}},
        _State,
        #{ disc_cb_list := DCBL } = Data
    ) ->
    Data2 = Data#{ disc_cb_list => lists:append(DCBL, [{Name, DisconnectCallback}]) },
    {keep_state, Data2, [{reply, From, ok}]};
handle_event(
        {call, From},
        {remove_connect_callback, Name},
        _State,
        #{ conn_cb_list := CCBL } = Data
    ) ->
    Data2 = Data#{ conn_cb_list => lists:keydelete(Name, 1, CCBL)},
    {keep_state, Data2, [{reply, From, ok}]};
handle_event(
        {call, From},
        {remove_disconnect_callback, Name},
        _State,
        #{ disc_cb_list := DCBL } = Data
    ) ->
    Data2 = Data#{ disc_cb_list => lists:keydelete(Name, 1, DCBL)},
    {keep_state, Data2, [{reply, From, ok}]};
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
        #{
            node := Node,
            conn_cb_list := ConnectCallbacks
        } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    ok = run_callbacks(connect, ConnectCallbacks),
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
        #{
            node := Node,
            disc_cb_list := DisconnectCallbacks
        } = Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    ok = run_callbacks(disconnect, DisconnectCallbacks),
    {next_state, disconnected, Data#{connected => false}};
% If Node fails or does not exist, the message {nodedown, Node} is delivered to the process.
% This might be sent from hawk_node_mon a second time round. Debug why...
handle_event(
        info,
        {nodedown, _Node},
        disconnected,
        Data
    ) ->
    ?LOG_NOTICE(#{data => Data}),
    keep_state_and_data;
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
            % ok = hawk_node_mon2:add_node(hawk_nodes_sup:id(Node), self()),
            Data2 = Data#{
                backoff_type => hawk_config:backoff_type(),
                backoff_wait => hawk_config:backoff_wait(),
                connected => true
            },
            % {next_state, connected, Data2};
            {keep_state, Data2};
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

terminate(shutdown, State, #{disc_cb_list := DisconnectCallbacks} = Data) ->
    ok = run_callbacks(disconnect, DisconnectCallbacks),
    ?LOG_NOTICE(#{
        terminate_state => State,
        terminate_data => Data
    });
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

run_callbacks(Type, Callbacks) ->
    Async = hawk_config:callbacks_async(),
    ShowCallbackStacktrace = hawk_config:show_callback_stacktrace(),
    lists:foreach(fun({Name, F}) ->
        ?LOG_INFO(#{
            start_callback => Name,
            type => Type
        }),
        %% TODO: Maybe log callback fun somewhere as info/debug
        %% TODO: maybe spawn, or allow to specify whether callbacks may block or not...
        try
            case Async of
                true ->
                    spawn(F);
                false ->
                    F()
            end,
            ?LOG_INFO(#{
                end_of_callback => Name,
                type => Type
            })
        catch
            C:E:S ->
                Log = #{
                    failed_callback => Name,
                    type => Type,
                    c => C,
                    e => E
                },
                ToLog = case ShowCallbackStacktrace of
                    true ->
                        Log#{s => S};
                    false ->
                        Log
                end,
                ?LOG_ERROR(ToLog)
        end
    end, Callbacks).
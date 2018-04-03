-module(hawk_node).

-export([
    start_link/4,
    do_start_link/4
]).

%% TODO:
%% - The ReqPid replies has to be changed to only reply ( ReqPid ! ok ) when the state has changed. ( investigate )

-spec start_link(node(), atom(), list(), list()) -> {ok, pid()}.
start_link(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    proc_lib:start_link(?MODULE, do_start_link,
        [Node, Cookie, ConnectedCallback, DisconnectedCallback]).

do_start_link(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    State = initial_state(Node, Cookie, ConnectedCallback, DisconnectedCallback),
    true = erlang:register(hawk_nodes_sup:id(Node), self()),
    % ok = hawk_node_mon:add_node(Node,Cookie),
    ok = hawk_node_mon:add_node(Node),
    ok = proc_lib:init_ack({ok, self()}),
    do_wait(State).

% -spec initial_state(node(), atom(), hawk:callbacks(), hawk:callbacks()) -> map().
initial_state(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks) ->
    #{ connected=>false,
       node=>Node,
       cookie=>Cookie,
       conn_cb_list=>ConnectedCallbacks,
       disc_cb_list=>DisconnectedCallbacks,
       connection_retries=>application:get_env(hawk, connection_retries, 600),
       conn_retry_wait=>application:get_env(hawk, conn_retry_wait, 100)
    }.

%% TODO: maybe configure for auto execute callbacks, based on current state.
%% sometimes you do not want the callbacks to be executed immediately.
% -spec do_wait(map()) -> ok.
do_wait(#{ connection_retries := ConnTries, node := Node }) when ConnTries =< 0 ->
    error_logger:info_msg("max connection attempts: dropping ~p soon~n", [Node]),
    spawn(fun() -> ok = hawk:remove_node(Node) end),
    deathbed();
do_wait(#{ connection_retries := ConnTries, conn_retry_wait := ConnTryWait, connected := false,
           node := Node, cookie := Cookie, conn_cb_list := CCBL, disc_cb_list := DCBL } = State) when ConnTries > 0 ->
    % io:format("HAWK_NODE DO_WAIT!!!~n"),
    true = erlang:set_cookie(Node,Cookie),
    case net_kernel:connect_node(Node) of
        true ->
            % Fake send ourselves a {nodeup,...} message.
            % Hidden/Slave Nodes are not sending {nodeup, ...} messages.
            % io:format("Going to send a nodeup myself~n"),
            self() ! {nodeup, Node},
            ok;
        false ->
            ok
    end,
    receive
        % We get these, once the node startups up.
        {nodeup, Node} ->
            % error_logger:error_msg("do_wait {nodeup, Node} ~p ~p~n", [{nodeup, Node}, erlang:process_info(self(), registered_name)]),
            process_flag(trap_exit, true),
            %io:format("do_wait {nodeup, Node}~n"),
            connected_callback(CCBL),
            loop(State#{ connected => true });
        {nodedown, Node} ->
            % error_logger:error_msg("do_wait {nodedown, Node} ~p ~p~n", [{nodedown, Node}, erlang:process_info(self(), registered_name)]),
            do_wait(State#{ connected => false, connection_retries => ConnTries-1 });
        {call, state, ReqPid} ->
            ReqPid ! {response, State},
            loop(State);
        %% Not connected yet, cannot call connect callback
        {call, {add_connect_callback, {Name,ConnectCallback}}, ReqPid} when is_function(ConnectCallback) ->
            do_wait(case lists:keyfind(Name, 1, CCBL) of
                false ->
                    ReqPid ! {response, updated},
                    State#{conn_cb_list => [{Name,ConnectCallback}|CCBL]};
                _ ->
                    ReqPid ! {response, duplicate},
                    State
            end);
        %% Not connected yet, can call disconnect callback
        {call, {add_disconnect_callback, {Name,DisconnectCallback}}, ReqPid} when is_function(DisconnectCallback) ->
            do_wait(case lists:keyfind(Name, 1, DCBL) of
                false ->
                    ok = disconnect_or_delete_callback([{Name,DisconnectCallback}]),
                    ReqPid ! {response, updated},
                    State#{disc_cb_list => [{Name,DisconnectCallback}|DCBL]};
                _ ->
                    ReqPid ! {response, duplicate},
                    State
            end);
        {call, {remove_connect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            loop(State#{conn_cb_list => lists:keydelete(Name, 1, CCBL)});
        {call, {remove_disconnect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            loop(State#{disc_cb_list => lists:keydelete(Name, 1, DCBL)});
        {call,_,ReqPid} ->
            ReqPid ! {response, connecting},
            do_wait(State);
        {system, From, _Msg = get_state} ->
            {_, _} = gen:reply(From, State),
            loop(State);
        {'EXIT', Who, shutdown} ->
            do_terminate(Who, DCBL, State, do_wait);
        Else ->
            error_logger:error_msg("do_wait received:~p ~p~n", [Else, erlang:process_info(self(), registered_name)]),
            do_wait(State)
    after
        ConnTryWait ->
            % io:format("Waited long enough ... ... ~n", []),
            do_wait(State#{connection_retries => ConnTries - 1})
    end.

% -spec loop(map()) -> ok.
loop(#{conn_cb_list := CCBL, disc_cb_list := DCBL, node := Node} = State) ->
    %io:format("HAWK_NODE LOOP!!!~n"),
    receive
        {'EXIT', Who, shutdown} ->
            do_terminate(Who, DCBL, State, loop);
        {nodeup, Node} ->
            % error_logger:error_msg("loop {nodeup, Node} ~p ~p~n", [{nodedown, Node}, erlang:process_info(self(), registered_name)]),
            loop(State);
        {nodedown, Node} ->
            % error_logger:error_msg("loop {nodedown, Node} ~p ~p~n", [{nodedown, Node}, erlang:process_info(self(), registered_name)]),
            ok = disconnect_or_delete_callback(DCBL),
            do_wait(State#{ connected => false });
        {call, state, ReqPid} ->
            ReqPid ! {response, State},
            loop(State);
        %% Connected so we can execute the connected callback
        {call, {add_connect_callback, {Name,ConnectCallback}}, ReqPid} when is_function(ConnectCallback) ->
            loop(case lists:keyfind(Name, 1, CCBL) of
                false ->
                    connected_callback([{Name,ConnectCallback}]),
                    ReqPid ! {response, updated},
                    State#{conn_cb_list => [{Name,ConnectCallback}|CCBL]};
                _ ->
                    ReqPid ! {response, duplicate},
                    State
            end);
        {call, {add_disconnect_callback, {Name,DisconnectCallback}}, ReqPid} when is_function(DisconnectCallback) ->
            loop(case lists:keyfind(Name, 1, DCBL) of
                false ->
                    ReqPid ! {response, updated},
                    State#{disc_cb_list => [{Name,DisconnectCallback}|DCBL]};
                _ ->
                    ReqPid ! {response, duplicate},
                    State
            end);
        {call, {remove_connect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            loop(State#{conn_cb_list => lists:keydelete(Name, 1, CCBL)});
        {call, {remove_disconnect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            loop(State#{disc_cb_list => lists:keydelete(Name, 1, DCBL)});
        {call, callbacks, ReqPid} ->
            ReqPid ! {response, {CCBL, DCBL}},
            loop(State);
        {system, From, _Msg = get_state} ->
            {_, _} = gen:reply(From, State),
            loop(State);
        Any ->
            error_logger:error_msg("loop pid recv : ~p~n", [Any, erlang:process_info(self(), registered_name)]),
            loop(State)
    after
        % Hidden nodes are not sending {nodedown,...} messages.
        1000 ->
            case rpc:call(Node, erlang, localtime, []) of
                {badrpc,nodedown} ->
                    ok = disconnect_or_delete_callback(DCBL),
                    do_wait(State#{ connected => false });
                _X ->
                    loop(State)
            end
    end.

% -spec do_terminate(pid(), hawk:callbacks(), map(), loop | do_wait) -> ok.
do_terminate(Who, DCBL, State, LoopFunctionName) when LoopFunctionName == loop orelse
                                                      LoopFunctionName == do_wait ->
    %io:format("HAWK_NODE DO_TERMINATE!!!~n"),
    error_logger:info_msg(
        "requested shutdown from ~p name:~p~n",
        [Who,erlang:process_info(Who, registered_name)]),
    case whereis(hawk_nodes_sup) == Who of
        true ->
            % io:format("SHUTDOWN WAS FROM NODES SUP!!!~n", []),
            ok = disconnect_or_delete_callback(DCBL),
            exit(self());
        false ->
            % io:format("SHUTDOWN WAS  ... NOT  FROM NODES SUP!!!~n", []),
            LoopFunctionName(State)
    end.

% Required for a controlled removal of the node. So that supervisor doesn't just restart.
deathbed() ->
    receive
        _ -> deathbed()
    end.

% -spec connected_callback(hawk:callbacks()) -> ok.
connected_callback(CCBL) ->
    lists:foreach(fun({_Name, F}) ->
        %% TODO: Maybe log callback fun somewhere as info/debug
        %% TODO: maybe spawn, or allow to specify whether callbacks may block or not...
        try
            F()
        catch
            C:E ->
                error_logger:error_msg("hawk_node connected callback failed ~p ~p ~p",
                    [C, E, erlang:get_stacktrace()])
        end
    end, CCBL).

% -spec disconnect_or_delete_callback(hawk:callbacks()) -> ok.
disconnect_or_delete_callback(DCBL) ->
    lists:foreach(fun({_Name, F}) ->
    %% TODO: Maybe log callback fun somewhere as info/debug
    %% TODO: maybe spawn, or allow to specify whether callbacks may block or not...
        try
            F()
        catch
            C:E ->
                error_logger:error_msg("hawk_node disconnected callback failed ~p ~p ~p",
                    [C, E, erlang:get_stacktrace()])
        end
    end, DCBL).
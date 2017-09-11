-module(hawk_node).

-export([
    start_link/4
]).

start_link(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    State = initial_state(Node, Cookie, ConnectedCallback, DisconnectedCallback),
    {ok, proc_lib:spawn_link(fun() ->
        true = erlang:register(Node, self()),
        ok = hawk_node_mon:add_node(Node,Cookie),
        do_wait(State)
    end)}.

%% TODO: maybe configure for auto execute callbacks, based on current state.
%% sometimes you do not want the callbacks to be executed immediately.
do_wait(#{ connection_retries := ConnTries, node := Node }) when ConnTries =< 0 ->
    error_logger:info_msg("max connection attempts: dropping ~p soon~n", [Node]),
    spawn(fun() -> ok = hawk:remove_node(Node) end),
    deathbed();
do_wait(#{ connection_retries := ConnTries, conn_retry_wait := ConnTryWait, connected := false,
           node := Node, cookie := Cookie, conn_cb_list := CCBL, disc_cb_list := DCBL } = State) when ConnTries > 0 ->
    ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect_node(Node))),
    receive
        {nodeup, Node} ->
            % error_logger:error_msg("do_wait {nodeup, Node} ~p ~p~n", [{nodeup, Node}, erlang:process_info(self(), registered_name)]),
            process_flag(trap_exit, true),
            connected_callback(CCBL),
            loop(State);
        {nodedown, Node} ->
            % error_logger:error_msg("do_wait {nodedown, Node} ~p ~p~n", [{nodedown, Node}, erlang:process_info(self(), registered_name)]),
            do_wait(State#{ connected => false, connection_retries => ConnTries-1 });
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
        {'EXIT',Who,shutdown} ->
            do_terminate(Who, DCBL, State, loop);
        Else ->
            error_logger:error_msg("do_wait received:~p ~p~n", [Else, erlang:process_info(self(), registered_name)]),
            do_wait(State)
    after
        ConnTryWait ->
            do_wait(State#{connection_retries => ConnTries - 1})
    end.

loop(#{conn_cb_list := CCBL, disc_cb_list := DCBL, node := Node} = State) ->
    receive
        {nodeup, Node} ->
            % error_logger:error_msg("loop {nodeup, Node} ~p ~p~n", [{nodedown, Node}, erlang:process_info(self(), registered_name)]),
            loop(State#{connected => true});
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
        {'EXIT',Who,shutdown} ->
            do_terminate(Who, DCBL, State, loop);
        Any ->
            error_logger:error_msg("loop pid recv : ~p~n", [Any, erlang:process_info(self(), registered_name)]),
            loop(State#{})
    end.

do_terminate(Who, DCBL, State, LoopFunctionName) ->
    error_logger:info_msg(
        "requested shutdown from ~p name:~p~n",
        [Who,erlang:process_info(Who, registered_name)]),
    case whereis(hawk_nodes_sup) == Who of
        true ->
            ok = disconnect_or_delete_callback(DCBL);
        false ->
            LoopFunctionName(State)
    end.

deathbed() ->
    receive
        _ -> deathbed()
    end.

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

initial_state(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks) ->
    #{ connected=>false,
       node=>Node,
       cookie=>Cookie,
       conn_cb_list=>ConnectedCallbacks,
       disc_cb_list=>DisconnectedCallbacks,
       %% 600 attempts, at 100ms each, 60 seconds default
       connection_retries=>application:get_env(hawk, connection_retries, 600),
       conn_retry_wait=>application:get_env(hawk, conn_retry_wait, 100)
    }.
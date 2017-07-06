-module(hawk_node).

-export([
    start_link/4
]).

start_link(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    State = initial_state(Node, Cookie, ConnectedCallback, DisconnectedCallback),
    {ok, proc_lib:spawn_link(fun() ->
        true = erlang:register(Node, self()),
        connecting(State)
    end)}.

connecting(#{ connected := false, node := Node, cookie :=
              Cookie } = State) ->
    LoopPid = self(),
    RetryCount = application:get_env(hawk, connection_retries, 1000),
    P = proc_lib:spawn_link(fun() -> do_rem_conn(LoopPid, Node, Cookie, RetryCount) end),
    do_wait(State#{do_rem_conn_pid => P}).

%% TODO: maybe configure for auto execute callbacks, based on current state.
%% sometimes you do not want the callbacks to be executed immediately.

do_wait(#{ connected := false, node := Node, conn_cb_list := CCBL, disc_cb_list := DCBL } = State) ->
    receive
        connected ->
            process_flag(trap_exit, true),
            true = erlang:monitor_node(Node, true),
            connected_callback(CCBL),
            loop(State#{connected => true});
        max_connection_attempts ->
            error_logger:info_msg("max connection attempts: dropping ~p soon~n", [Node]),
            % Not sure, but removing it for now, since it was running twice, once for disconnect, and for reaching max attempts
            % ok = disconnect_or_delete_callback(DCBL),
            spawn(fun() -> ok = hawk:remove_node(Node) end),
            deathbed();
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
            error_logger:error_msg("connecting received:~p~n", [Else]),
            do_wait(State)
    end.

loop(#{connected := true, conn_cb_list := CCBL, disc_cb_list := DCBL, node := Node} = State) ->
    #{ do_rem_conn_pid := DoRemConnPid } = State,
    receive
        {nodedown, Node} ->
            ok = disconnect_or_delete_callback(DCBL),
            connecting(State#{ connected => false });
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
        {'EXIT',DoRemConnPid,normal} -> %% Connecting proc dies, because it's done/connected ( started with link )
            loop(State#{do_rem_conn_pid=>undefined});
        Any ->
            error_logger:error_msg("loop pid recv : ~p~n", [Any]),
            loop(State#{})
    end.

do_terminate(Who, DCBL, State, LoopFunctionName) ->
    error_logger:info_msg(
        "requested shutdown from ~p name:~p~n",
        [Who,erlang:process_info(Who, registered_name)]),
    case whereis(hawk_sup)==Who of
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
       disc_cb_list=>DisconnectedCallbacks
    }.

do_rem_conn(ConnectingPid, _, _, 0) ->
    ConnectingPid ! max_connection_attempts;
%% Setting retry count -1, will just always try to reconnect...
do_rem_conn(ConnectingPid, Node, Cookie, RetryCount) -> %% Find a more ellegant way of waiting...
    case ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect_node(Node))) of
        false ->
            Wait = application:get_env(hawk, conn_retry_wait, 50),
            % timer:sleep(application:get_env(hawk, conn_retry_wait, 50)),
            receive
                {call,_,ReqPid} ->
                ReqPid ! {response, connecting},
                do_rem_conn(ConnectingPid, Node, Cookie, RetryCount-1)
            after
                Wait ->
                    do_rem_conn(ConnectingPid, Node, Cookie, RetryCount-1)
            end;
        true ->
            error_logger:info_msg("Node ~p connected...", [Node]),
            ConnectingPid ! connected
    end.


-module(hawk_node).

-export([
    start_link/4
]).

start_link(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    %% io:format("~p start_link ~n", [?MODULE]),
    State = initial_state(Node, Cookie, ConnectedCallback, DisconnectedCallback),
    {ok, proc_lib:spawn_link(fun() ->
        true = erlang:register(Node, self()),
        connecting(State)
    end)}.

connecting(#{ connected := false, node := Node, cookie :=
              Cookie, conn_cb_list := CCBL, disc_cb_list := DCBL } = State) ->
    % io:format("c"),
    LoopPid = self(),
    RetryCount = application:get_env(hawk, connection_retries, 1000),
    proc_lib:spawn_link(fun() -> do_rem_conn(LoopPid, Node, Cookie, RetryCount) end),
    do_wait(State).

do_wait(#{ connected := false, node := Node, cookie := Cookie,
           conn_cb_list := CCBL, disc_cb_list := DCBL } = State) ->
    receive
        connected ->
            process_flag(trap_exit, true),
            connected_callback(CCBL),
            true = erlang:monitor_node(Node, true),
            loop(State#{connected => true });
        max_connection_attempts ->
            io:format("max connection attempts: dropping ~p soon~n", [Node]),
            ok = disconnect_or_delete_callback(DCBL),
            spawn(fun() -> ok = hawk:remove_node(Node) end),
            deathbed();
        %% Handle the callbacks add/del when not connected ( connecting )
        {call, {add_connect_callback, {Name,ConnectCallback}}, ReqPid} when is_function(ConnectCallback) ->
            ReqPid ! {response, updated},
            do_wait(State#{conn_cb_list => [{Name,ConnectCallback}|CCBL]});
        {call, {add_disconnect_callback, {Name,DisconnectCallback}}, ReqPid} when is_function(DisconnectCallback) ->

            %% Since we are not connected, we can execute the disconnected callback.
            ok = disconnect_or_delete_callback([{Name,DisconnectCallback}]),

            ReqPid ! {response, updated},
            do_wait(State#{disc_cb_list => [{Name,DisconnectCallback}|DCBL]});
        {call, {remove_connect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            do_wait(State#{conn_cb_list => lists:keydelete(Name, 1, CCBL)});
        {call, {remove_disconnect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            do_wait(State#{disc_cb_list => lists:keydelete(Name, 1, DCBL)});
        {call,_,ReqPid} ->
            ReqPid ! {response, connecting},
            do_wait(State);
        Else ->
            io:format("connecting received:~p~n", [Else]),
            do_wait(State)
    end.

loop(#{connected := true, conn_cb_list := CCBL, disc_cb_list := DCBL, node := Node } = State) ->
    % io:format("l"),
    receive
        {nodedown, Node} ->
            ok = disconnect_or_delete_callback(DCBL),
            connecting(State#{ connected => false });
        {call, state, ReqPid} ->
            ReqPid ! {response, State},
            loop(State);
        {call, {add_connect_callback, {Name,ConnectCallback}}, ReqPid} when is_function(ConnectCallback) ->

            %% Since we are connected, we can trigger the connect callback
            connected_callback([{Name,ConnectCallback}]),

            ReqPid ! {response, updated},
            loop(State#{conn_cb_list => [{Name,ConnectCallback}|CCBL]});
        {call, {add_disconnect_callback, {Name,DisconnectCallback}}, ReqPid} when is_function(DisconnectCallback) ->
            ReqPid ! {response, updated},
            loop(State#{disc_cb_list => [{Name,DisconnectCallback}|DCBL]});
        {call, {remove_connect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            loop(State#{conn_cb_list => lists:keydelete(Name, 1, CCBL)});
        {call, {remove_disconnect_callback, Name}, ReqPid} ->
            ReqPid ! {response, updated},
            loop(State#{disc_cb_list => lists:keydelete(Name, 1, DCBL)});
        {call, callbacks, ReqPid} ->
            ReqPid ! {response, {CCBL, DCBL}},
            loop(State);
        {'EXIT',Pid,shutdown} ->
            io:format("requested shutdown from ~p name:~p~n", [Pid,erlang:process_info(Pid, registered_name)]),
            ok = disconnect_or_delete_callback(DCBL);
        Any ->
            io:format("loop pid recv : ~p~n", [Any]),
            loop(State)
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
                io:format("hawk_node connected callback failed ~p ~p ~p",
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
                io:format("hawk_node disconnected callback failed ~p ~p ~p",
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
    %% io:format("~p ", [RetryCount]),
    case ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node))) of
        false ->
            timer:sleep(application:get_env(hawk, conn_retry_wait, 50)),
            do_rem_conn(ConnectingPid, Node, Cookie, RetryCount-1);
        true ->
            ConnectingPid ! connected
    end.


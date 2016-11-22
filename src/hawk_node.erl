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

connecting(#{ connected := false, node := Node, cookie := Cookie, conn_cb_list := CCBL } = State) ->
    % io:format("c"),
    LoopPid = self(),
    proc_lib:spawn_link(fun() -> do_rem_conn(LoopPid, Node, Cookie) end),
    receive
        connected ->
            process_flag(trap_exit, true),
            connected_callback(CCBL),
            true = erlang:monitor_node(Node, true),
            loop(State#{connected => true });
        Else ->
            io:format("Else : ~p~n", [Else])
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
            ReqPid ! {response, updated},
            loop(State#{conn_cb_list => [{Name,ConnectCallback}|CCBL]});
        {call, {add_disconnect_callback, {Name,DisconnectCallback}}, ReqPid} when is_function(DisconnectCallback) ->
            ReqPid ! {response, updated},
            loop(State#{disc_cb_list => [{Name,DisconnectCallback}|DCBL]});
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

do_rem_conn(ConnectingPid, Node, Cookie) -> %% Find a more ellegant way of waiting...
    % io:format("d"),
    case ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node))) of
        false ->
            timer:sleep(application:get_env(hawk, conn_retry_wait, 200)),
            do_rem_conn(ConnectingPid, Node, Cookie);
        true ->
            ConnectingPid ! connected
    end.
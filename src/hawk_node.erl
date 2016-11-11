-module(hawk_node).

-export([
    start_link/4
]).

start_link(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    io:format("~p start_link ~n", [?MODULE]),
    State = initial_state(Node, Cookie, ConnectedCallback, DisconnectedCallback),
    {ok, proc_lib:spawn_link(fun() -> 
        true = erlang:register(Node, self()),
        connecting(State)
    end)}.

connecting(#{ connected := false, node := Node, cookie := Cookie, conn_cb := CCB } = State) ->
    % io:format("c"),
    LoopPid = self(),
    proc_lib:spawn_link(fun() -> do_rem_conn(LoopPid, Node, Cookie) end),
    receive
        connected ->
            ok = CCB(Node, Cookie),
            true = erlang:monitor_node(Node, true),
            loop(State#{connected => true });
        Else ->
            io:format("Else : ~p~n", [Else])
    end.

loop(#{connected := true, disc_cb := DCB } = State) ->
    % io:format("l"),
    receive
        {nodedown, Node} ->
            % io:format("{nodedown, ~p}~n", [Node]),
            ok = DCB(Node),
            connecting(State#{ connected => false });
        {call, state, ReqPid} ->
            ReqPid ! {response, State},
            loop(State);
        Any ->
            io:format("loop pid recv : ~p~n", [Any]),
            loop(State)
    end.

initial_state(Node, Cookie, ConnectedCallback, DisconnectedCallback) ->
    #{ connected=>false, 
       node=>Node, 
       cookie=>Cookie,
       conn_cb=>ConnectedCallback, 
       disc_cb=>DisconnectedCallback
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
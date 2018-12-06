-module(hawk_node_mon).
-export([
    start_link/0,
    do_start_link/0,
    add_node/2
]).

-ifdef(TEST).
-export([
    loop/0
]).
-endif.

start_link() ->
    proc_lib:start_link(?MODULE, do_start_link, []).

do_start_link() ->
    process_flag(trap_exit, true),
    true = erlang:register(?MODULE, self()),
    ?MODULE = ets:new(?MODULE, [named_table, private, ordered_set]),
    ok = net_kernel:monitor_nodes(true),
    ok = proc_lib:init_ack({ok, self()}),
    loop().

add_node(Node, NodePid) ->
    whereis(?MODULE) ! {add_node, Node, NodePid, self()},
    receive
        ok ->
            ok
    after
        5000 ->
            timeout
    end.

% is_known_node(Node) ->
%     ok.

loop() ->
    receive
        {add_node, Node, NodePid, ReqPid} ->
            error_logger:error_msg("add_node -> Node:~p NodePid:~p~n~n",
                      [Node, NodePid]),
            true = erlang:monitor_node(Node, true),
            true = erlang:link(NodePid),
            % error_logger:error_msg("", []),
            ReqPid ! ok,
            loop();
        {nodeup, Node} ->
            % true = ets:insert(?MODULE,
            ok = case whereis(hawk_nodes_sup:id(Node)) of
                undefined -> % Another node connected to our cluster...
                    error_logger:error_msg("!nodeup -> Node:~p not hawk node SYSTEM_TIME:~p~n",
                                           [Node, erlang:system_time()]);
                Pid ->
                    error_logger:error_msg("nodeup -> Node:~p NodePid:~p~n~n",
                              [Node, Pid]),
                    Pid ! {nodeup, Node},
                    ok
            end,
            loop();
        {nodedown, Node} ->
            ok = case whereis(hawk_nodes_sup:id(Node)) of
                undefined ->
                    error_logger:error_msg("!nodedown -> Node:~p not registered as ~p~n",
                                           [Node, hawk_nodes_sup:id(Node)]);
                Pid ->
                    error_logger:error_msg("nodedown -> Node:~p NodePid:~p~n~n",
                              [Node, Pid]),
                    Pid ! {nodedown, Node},
                    ok
            end,
            loop();
        Msg ->
            error_logger:error_msg(
                "~p received ~p~n",
                [?MODULE, Msg]
            ),
            loop()
    end.
-module(hawk_node_mon).
-export([
    start_link/0,
    do_start_link/0,
    add_node/1
]).

start_link() ->
    proc_lib:start_link(?MODULE, do_start_link, []).

do_start_link() ->
    process_flag(trap_exit, true),
    true = erlang:register(?MODULE, self()),
    ?MODULE = ets:new(?MODULE, [named_table, private, ordered_set]),
    ok = net_kernel:monitor_nodes(true),
    ok = proc_lib:init_ack({ok, self()}),
    loop().

add_node(Node) ->
    whereis(?MODULE) ! {add_node, Node},
    ok.

% is_known_node(Node) ->
%     ok.

loop() ->
    receive
        {add_node, Node} ->
            % io:format("add_node -> Node:~p NodePid:~p~n~n",
            %           [Node, whereis(hawk_nodes_sup:id(Node))]),
            true = erlang:monitor_node(Node, true),
            loop();
        {nodeup, Node} ->
            % true = ets:insert(?MODULE,
            case whereis(hawk_nodes_sup:id(Node)) of
                undefined -> % Another node connected to our cluster...
                    error_logger:error_msg("!nodeup -> Node:~p not hawk node SYSTEM_TIME:~p~n",
                                           [Node, erlang:system_time()]);
                Pid ->
                    % io:format("nodeup -> Node:~p NodePid:~p~n~n",
                    %           [Node, Pid]),
                    Pid ! {nodeup, Node}
            end,
            loop();
        {nodedown, Node} ->
            case whereis(hawk_nodes_sup:id(Node)) of
                undefined ->
                    error_logger:error_msg("!nodedown -> Node:~p not hawk node~n",
                                           [Node]);
                Pid ->
                    % io:format("nodedown -> Node:~p NodePid:~p~n~n",
                    %           [Node, Pid]),
                    Pid ! {nodedown, Node}
            end,
            loop();
        Msg ->
            error_logger:error_msg(
                "~p received ~p~n",
                [?MODULE, Msg]
            ),
            loop()
    end.
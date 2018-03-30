-module(hawk_node_mon).
-export([
    start_link/0,
    do_start_link/0,
    add_node/2
]).

start_link() ->
    proc_lib:start_link(?MODULE, do_start_link, []).

do_start_link() ->
    ok = net_kernel:monitor_nodes(true),
    true = erlang:register(?MODULE, self()),
    ok = proc_lib:init_ack({ok, self()}),
    loop([]).

add_node(Node, Cookie) ->
    whereis(?MODULE) ! {add_node, Node, Cookie},
    ok.

loop(Nodes) ->
    receive
        _M={add_node, Node, _Cookie} ->
            %% io:format("A : ~p~n", [M]),
            true = erlang:monitor_node(Node, true),
            loop([Node|Nodes]);
        _M={nodeup, Node} ->
            %% io:format("B : ~p~n", [M]),
            whereis(Node) ! {nodeup, Node},
            loop(Nodes);
        _M={nodedown, Node} ->
            %% io:format("C : ~p~n", [M]),
            whereis(Node) ! {nodedown, Node},
            loop(Nodes);
        A ->
            error_logger:error_msg(
                "~p received ~p~n",
                [?MODULE, A]
            ),
            loop(Nodes)
    end.
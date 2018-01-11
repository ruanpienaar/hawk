-module(hawk_node_mon).
-export([
    start_link/0,
    add_node/2
]).

start_link() ->
    {ok, proc_lib:spawn_link(fun() ->
        ok = net_kernel:monitor_nodes(true),
        true = erlang:register(?MODULE, self()),
        loop([])
    end)}.

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
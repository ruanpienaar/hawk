-module(hawk2_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Tests
%% add non existing node, then remove
%% add non existing node, then add node with PEER.
-export([
    add_node1/1,
    add_node2/1,
    add_both_nodes/1,
    node_callbacks/1
]).

all() ->
    [
        add_node1,
        add_node2,
        add_both_nodes,
        node_callbacks
    ].

init_per_suite(Config) ->
    % _ = ets:new(test_table, [named_table, public, ordered_set]),
    {ok, _} = dbg:tracer(),
    {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(hawk_node_mon, cx),
    % {ok, _} = dbg:tpl(hawk_node2, cx),
    {ok, _} = dbg:tpl(hawk_node2, cx),
    % {ok, _} = dbg:tpl(hawk_nodes_sup, cx),
    {ok, _} = erlang_testing:start_distrib('hawk_SUITE@localhost', shortnames),
    true = auth:set_cookie(hawk),
    ok = application:start(hawk),
    Config.

end_per_suite(Config) ->
    ok = dbg:stop(),
    Config.

init_per_testcase(_TestCase, Config) ->
    _ = ets:new(test_table, [named_table, public, ordered_set]),
    true = ets:delete_all_objects(test_table),
    NodeName1 = 'node1',
    NodeName2 = 'node2',
    PeerName1 = ?CT_PEER_NAME(NodeName1),
    PeerName2 = ?CT_PEER_NAME(NodeName2),
    % ct:pal("PeerName1 ~p\n", [PeerName1]),
    % ct:pal("PeerName2 ~p\n", [PeerName2]),
    {ok, Peer1, Node1} = ?CT_PEER(#{
        longnames => false,
        name => PeerName1,
        host => "localhost",
        args => ["-setcookie", "cookie"],
        connection => standard_io
    }),
    {ok, Peer2, Node2} = ?CT_PEER(#{
        longnames => false,
        name => PeerName2,
        host => "localhost",
        args => ["-setcookie", "cookie"],
        connection => standard_io
    }),
    % ct:pal("Peer ~p Node ~p\n", [Peer1, Node1]),
    % ct:pal("Peer ~p Node ~p\n", [Peer2, Node2]),
    running = peer:get_state(Peer1),
    running = peer:get_state(Peer2),
    % Make sure they're not connected yet
    [] = nodes(),
    [] = nodes(hidden),
    [{nodes, [{Peer1, Node1}, {Peer2, Node2}]}| Config].

end_per_testcase(_TestCase, Config) ->
    {nodes, [{Peer1, Node1}, {Peer2, Node2}]} = lists:keyfind(nodes, 1, Config),

    hawk_node2:going_to_remove_node(Node1),
    hawk_node2:going_to_remove_node(Node2),

    _ = hawk_nodes_sup:delete_child(Node1),
    _ = hawk_nodes_sup:delete_child(Node2),

    ok = peer:stop(Peer1),
    ok = peer:stop(Peer2),
    [] = nodes(),
    [] = nodes(hidden),
    Config.

add_node1(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, _Node2}]} = lists:keyfind(nodes, 1, Config),
    %% ct:pal("TEST node_exists add node ~p\n", [Node1]),
    [] = nodes(),
    {ok, _} = hawk:add_node(Node1, cookie),
    %% ct:pal("TEST node_exists wait stage 2\n", []),
    F = fun() ->
        XX = hawk:node_state(Node1),
        %% ct:pal("wait for connected ~p\n", [XX]),
        case XX of
            {ok, {_, #{ connected := C} = _M}} ->
                C;
            _ ->
                false
        end
    end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    %% ct:pal("TEST node_exists wait stage 3\n", []),
    {ok, _Pid, []} = hawk:node_exists(Node1),
    %% ct:pal("TEST node_exists wait stage 4\n", []),
    {ok, {_, #{  conn_cb_list := [],
                 backoff_type := fixed,
                 backoff_wait := 100,
                 connected := true,
                 cookie := _Cookie,
                 disc_cb_list := [],
                 node := Node1}}} = hawk:node_state(Node1),
    [{Node1, true}] = hawk:nodes().

add_node2(Config) ->
    {nodes, [{_Peer1, _Node1}, {_Peer2, Node2}]} = lists:keyfind(nodes, 1, Config),
    %% ct:pal("TEST node_exists add node ~p\n", [Node2]),
    [] = nodes(),
    {ok, _} = hawk:add_node(Node2, cookie),
    %% ct:pal("TEST node_exists wait stage 2\n", []),
    F = fun() ->
        XX = hawk:node_state(Node2),
        %% ct:pal("wait for connected ~p\n", [XX]),
        case XX of
            {ok, {_, #{ connected := C} = _M}} ->
                C;
            _ ->
                false
        end
    end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    %% ct:pal("TEST node_exists wait stage 3\n", []),
    {ok, _Pid, []} = hawk:node_exists(Node2),
    %% ct:pal("TEST node_exists wait stage 4\n", []),
    {ok, {_, #{  conn_cb_list := [],
                 backoff_type := fixed,
                 backoff_wait := 100,
                 connected := true,
                 cookie := _Cookie,
                 disc_cb_list := [],
                 node := Node2}}} = hawk:node_state(Node2),
    [{Node2, true}] = hawk:nodes().

add_both_nodes(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, Node2}]} = lists:keyfind(nodes, 1, Config),

    %% ct:pal("TEST node_exists add node ~p\n", [Node1]),
    [] = nodes(),
    {ok, _} = hawk:add_node(Node1, cookie),
    %% ct:pal("TEST node_exists wait stage 2\n", []),
    F = fun() ->
        XX = hawk:node_state(Node1),
        %% ct:pal("wait for connected ~p\n", [XX]),
        case XX of
            {ok, {_, #{ connected := C} = _M}} ->
                C;
            _ ->
                false
        end
    end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    %% ct:pal("TEST node_exists wait stage 3\n", []),
    {ok, _Pid, []} = hawk:node_exists(Node1),
    %% ct:pal("TEST node_exists wait stage 4\n", []),
    {ok, {_, #{  conn_cb_list := [],
                 backoff_type := fixed,
                 backoff_wait := 100,
                 connected := true,
                 cookie := _Cookie,
                 disc_cb_list := [],
                 node := Node1}}} = hawk:node_state(Node1),
    hawk_node2:end_of_test(Node1),

    [{Node1, true}] = hawk:nodes(),

    % timer:sleep(500),

    % START second node

    %% ct:pal("TEST node_exists add node ~p\n", [Node2]),
    [Node1] = nodes(),
    {ok, _} = hawk:add_node(Node2, cookie),
    %% ct:pal("TEST node_exists wait stage 2\n", []),
    F2 = fun() ->
        XX = hawk:node_state(Node2),
        %% ct:pal("wait for connected ~p\n", [XX]),
        case XX of
            {ok, {_, #{ connected := C} = _M}} ->
                C;
            _ ->
                false
        end
    end,
    ok = unit_testing:wait_for_match(10, F2, true, 100),
    %% ct:pal("TEST node_exists wait stage 3\n", []),
    {ok, _Pid2, []} = hawk:node_exists(Node2),
    %% ct:pal("TEST node_exists wait stage 4\n", []),
    {ok, {_, #{  conn_cb_list := [],
                 backoff_type := fixed,
                 backoff_wait := 100,
                 connected := true,
                 cookie := _Cookie2,
                 disc_cb_list := [],
                 node := Node2}}} = hawk:node_state(Node2),
    hawk_node2:end_of_test(Node2),

    [{Node1, true}, {Node2, true}] = lists:sort(hawk:nodes()),

    % timer:sleep(500),

    [Node1, Node2] = nodes(),


    % timer:sleep(500).
    %
    ok.

node_callbacks(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, _Node2}]} = lists:keyfind(nodes, 1, Config),
    %% ct:pal("TEST node_exists add node ~p\n", [Node1]),
    [] = nodes(),
    {ok, _} = hawk:add_node(
        Node1,
        cookie,
        [
            {add_1, fun() -> ets:insert(test_table,{1, Node1}) end},
            {add_2, fun() -> ets:insert(test_table,{2, Node1}) end},
            {add_3, fun() -> ets:insert(test_table,{3, Node1}) end}
        ],
        []
    ),
    %% ct:pal("TEST node_exists wait stage 2\n", []),
    F = fun() ->
        XX = hawk:node_state(Node1),
        %% ct:pal("wait for connected ~p\n", [XX]),
        case XX of
            {ok, {_, #{ connected := C} = _M}} ->
                C;
            _ ->
                false
        end
    end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    %% ct:pal("TEST node_exists wait stage 3\n", []),
    {ok, _Pid, [add_1, add_2, add_3]} = hawk:node_exists(Node1),
    %% ct:pal("TEST node_exists wait stage 4\n", []),
    {ok, {_, #{  conn_cb_list := [
                    {add_1, _},
                    {add_2, _},
                    {add_3, _}
                 ],
                 backoff_type := fixed,
                 backoff_wait := 100,
                 connected := true,
                 cookie := _Cookie,
                 disc_cb_list := [],
                 node := Node1}}} = hawk:node_state(Node1),
    [{Node1, true}] = hawk:nodes(),
    [
        {1, Node1},
        {2, Node1},
        {3, Node1}
    ] = ets:tab2list(test_table).
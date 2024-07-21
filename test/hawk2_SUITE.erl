-module(hawk2_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test

-export([
    all/0
    ,init_per_suite/1
    ,end_per_suite/1
    ,init_per_testcase/2
    ,end_per_testcase/2
]).

%% Tests
%% add non existing node, then remove
%% add non existing node, then add node with PEER.
-export([
    add_node1/1
    ,add_node2/1
    ,add_both_nodes/1
    ,add_node_then_remove/1
    ,node_callbacks_connect/1
    ,node_callbacks_disconnect/1
    % Test where node does not exist, hawk add node, start node, check connected
    % Test where node does not ecist, hawk add node, keep trying on fixed for 10 times
    % Test where node exist and peer closed node, keep trying, and node comes back
    % % Test where node exist and peer closed node, keep trying, and node does NOT come back
]).

all() ->
    [
        add_node1
        ,add_node2
        ,add_both_nodes
        ,add_node_then_remove
        ,node_callbacks_connect
        ,node_callbacks_disconnect
    ].

init_per_suite(Config) ->
    % _ = ets:new(test_table, [named_table, public, ordered_set]),
    % {ok, _} = dbg:tracer(),
    % {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(hawk_node_mon, cx),
    % {ok, _} = dbg:tpl(hawk_node2, cx),
    % {ok, _} = dbg:tpl(hawk_node2, cx),
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
    [] = nodes()++nodes(hidden),
    [{nodes, [{Peer1, Node1}, {Peer2, Node2}]}| Config].

end_per_testcase(_TestCase, Config) ->
    {nodes, [{Peer1, Node1}, {Peer2, Node2}]} = lists:keyfind(nodes, 1, Config),
    _ = hawk_nodes_sup:delete_child(Node1),
    _ = hawk_nodes_sup:delete_child(Node2),
    ok = peer:stop(Peer1),
    ok = peer:stop(Peer2),
    [] = nodes()++nodes(hidden),
    [] = nodes(hidden),
    Config.

add_node1(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, _Node2}]} = lists:keyfind(nodes, 1, Config),
    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node1, cookie),
    F = fun() -> element(1, hawk:node_state(Node1)) == connected end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    {ok, {[], []}} = hawk:node_exists(Node1),
    {
        connected,
        #{
            connected := true,
            node := Node1,
            cookie := cookie,
            conn_cb_list := [],
            backoff_type := fixed,
            backoff_wait := 100,
            disc_cb_list := [],
            connection_retries := 600
        }
    } = hawk:node_state(Node1),
    [Node1] = nodes()++nodes(hidden),
    [{Node1, true}] = hawk:nodes().

add_node2(Config) ->
    {nodes, [{_Peer1, _Node1}, {_Peer2, Node2}]} = lists:keyfind(nodes, 1, Config),
    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node2, cookie),
    F = fun() -> element(1, hawk:node_state(Node2)) == connected end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    {ok, {[], []}} = hawk:node_exists(Node2),
    {
        connected,
        #{
            connected := true,
            node := Node2,
            cookie := cookie,
            conn_cb_list := [],
            backoff_type := fixed,
            backoff_wait := 100,
            disc_cb_list := [],
            connection_retries := 600
        }
    } = hawk:node_state(Node2),
    [Node2] = nodes()++nodes(hidden),
    [{Node2, true}] = hawk:nodes().

add_both_nodes(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, Node2}]} = lists:keyfind(nodes, 1, Config),

    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node1, cookie),
    F = fun() -> element(1, hawk:node_state(Node1)) == connected end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    {ok, {[], []}} = hawk:node_exists(Node1),
    {
        connected,
        #{
            connected := true,
            node := Node1,
            cookie := cookie,
            conn_cb_list := [],
            backoff_type := fixed,
            backoff_wait := 100,
            disc_cb_list := [],
            connection_retries := 600
        }
    } = hawk:node_state(Node1),
    [Node1] = nodes()++nodes(hidden),
    [{Node1, true}] = hawk:nodes(),

    % START second node

    [Node1] = nodes()++nodes(hidden),
    [{Node1, true}] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node2, cookie),
    F2 = fun() -> element(1, hawk:node_state(Node2)) == connected end,
    ok = unit_testing:wait_for_match(10, F2, true, 100),
    {ok, {[], []}} = hawk:node_exists(Node2),
    {
        connected,
        #{
            connected := true,
            node := Node2,
            cookie := cookie,
            conn_cb_list := [],
            backoff_type := fixed,
            backoff_wait := 100,
            disc_cb_list := [],
            connection_retries := 600
        }
    } = hawk:node_state(Node2),
    [Node1, Node2] = nodes()++nodes(hidden),
    [{Node1, true}, {Node2, true}] = lists:sort(hawk:nodes()).

add_node_then_remove(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, _Node2}]} = lists:keyfind(nodes, 1, Config),
    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node1, cookie),
    F = fun() -> element(1, hawk:node_state(Node1)) == connected end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    {ok, {[], []}} = hawk:node_exists(Node1),
    {
        connected,
        #{
            connected := true,
            node := Node1,
            cookie := cookie,
            conn_cb_list := [],
            backoff_type := fixed,
            backoff_wait := 100,
            disc_cb_list := [],
            connection_retries := 600
        }
    } = hawk:node_state(Node1),
    [Node1] = nodes()++nodes(hidden),
    [{Node1, true}] = hawk:nodes(),

    ok = hawk:remove_node(Node1),

    FRem = fun() -> hawk_nodes_sup:children() end,
    ok = unit_testing:wait_for_match(10, FRem, [], 100),

    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),

    ok.

node_callbacks_connect(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, _Node2}]} = lists:keyfind(nodes, 1, Config),

    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),
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
    F = fun() -> element(1, hawk:node_state(Node1)) == connected end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    {
        ok,
        {
            [{add_1, _}, {add_2, _}, {add_3, _}],
            []
        }
    } = hawk:node_exists(Node1),
    {
        connected,
        #{
            connected := true,
            node := Node1,
            cookie := cookie,
            conn_cb_list := [{add_1, _}, {add_2, _}, {add_3, _}],
            backoff_type := fixed,
            backoff_wait := 100,
            disc_cb_list := [],
            connection_retries := 600
        }
    } = hawk:node_state(Node1),
    [{Node1, true}] = hawk:nodes(),
    [
        {1, Node1},
        {2, Node1},
        {3, Node1}
    ] = ets:tab2list(test_table).

node_callbacks_disconnect(Config) ->
    {nodes, [{_Peer1, Node1}, {_Peer2, _Node2}]} = lists:keyfind(nodes, 1, Config),

    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(
        Node1,
        cookie,
        [],
        [
            {remove_1, fun() -> ets:insert(test_table,{1, Node1}) end},
            {remove_2, fun() -> ets:insert(test_table,{2, Node1}) end},
            {remove_3, fun() -> ets:insert(test_table,{3, Node1}) end}
        ]
    ),
    F = fun() -> element(1, hawk:node_state(Node1)) == connected end,
    ok = unit_testing:wait_for_match(10, F, true, 100),
    {
        ok,
        {
            [],
            [{remove_1, _}, {remove_2, _}, {remove_3, _}]
        }
    } = hawk:node_exists(Node1),
    {
        connected,
        #{
            connected := true,
            node := Node1,
            cookie := cookie,
            conn_cb_list := [],
            backoff_type := fixed,
            backoff_wait := 100,
            disc_cb_list := [{remove_1, _}, {remove_2, _}, {remove_3, _}],
            connection_retries := 600
        }
    } = hawk:node_state(Node1),
    [{Node1, true}] = hawk:nodes(),
    [] = ets:tab2list(test_table),

    ok = hawk:remove_node(Node1),

    FRem = fun() -> hawk_nodes_sup:children() end,
    ok = unit_testing:wait_for_match(10, FRem, [], 100),

    [] = nodes()++nodes(hidden),
    [] = hawk:nodes(),

    [
        {1, Node1},
        {2, Node1},
        {3, Node1}
    ] = ets:tab2list(test_table).
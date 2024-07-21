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
-export([
    node_exists/1
]).

all() ->
    [
        node_exists
    ].

init_per_suite(Config) ->

    {ok, _} = dbg:tracer(),
    {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(hawk_node_mon, cx),
    % {ok, _} = dbg:tpl(hawk_node2, cx),

    {ok, _} = erlang_testing:start_distrib('hawk_SUITE@localhost', shortnames),
    true = auth:set_cookie(hawk),
    ok = application:start(hawk),
    Config.

end_per_suite(Config) ->
    ok = dbg:stop(),
    Config.

init_per_testcase(_TestCase, Config) ->
    % N1 = 'node1@localhost',
    % N2 = 'node2@localhost',
    Node1 = 'node1',
    % Node2 = 'node2',
    PeerName = ?CT_PEER_NAME(Node1),
    ct:pal("PeerName ~p\n", [PeerName]),
    {ok, Peer, Node} = ?CT_PEER(#{
        longnames => false,
        name => PeerName,
        host => "localhost",
        args => ["-setcookie", "cookie"],
        % detached => true,
        connection => standard_io
    }),
    ct:pal("Peer ~p Node ~p\n", [Peer, Node]),
    running = peer:get_state(Peer),
    [] = nodes(),
    % true = net_kernel:connect_node(Node),
    % true = net_kernel:connect_node(N1),
    % ct:pal("Peer net_kernel i ~p\n", [peer:call(Peer, net_kernel, i, [])]),
    % ct:pal("Peer net_kernel i ~p\n", [rpc:call(Node, net_kernel, i, [])]),
    % {ok, _Peer2, _Node2} = ?CT_PEER(#{name => ?CT_PEER_NAME(N2), cookie => cookie, detached => true}),
    % ok = rpc:call(Node, application, set_env, [kernel, key, value])
    [{nodes, [Node, undefined]}| Config].

end_per_testcase(_TestCase, Config) ->
    Config.

node_exists(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    ct:pal("TEST node_exists add node ~p\n", [N1]),
    [] = nodes(),
    {ok, _} = hawk:add_node(N1, cookie),
    ct:pal("TEST node_exists wait stage 2\n", []),
    F = fun() ->
        XX = hawk:node_state(N1),
        ct:pal("wait for connected ~p\n", [XX]),
        case XX of
            {ok, {_, #{ connected := C} = _M}} ->
                C;
            _ ->
                false
        end
    end,
    ok = unit_testing:wait_for_match(1, F, true, 1000),
    ct:pal("TEST node_exists wait stage 3\n", []),
    {ok,_Pid, []} = hawk:node_exists(N1),
    ct:pal("TEST node_exists wait stage 4\n", []),
    {ok, {_,#{conn_cb_list := [],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [],
                 node := N1}}} = hawk:node_state(N1).
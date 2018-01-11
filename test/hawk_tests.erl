-module(hawk_tests).

-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").

api_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            {"API -> nodes/0",
                fun hawk_nodes/0},
            {"API -> hawk_node_exists/1",
                fun hawk_node_exists/0},
            {"API -> add_node/2",
                fun add_node/0},
            {"API -> add_node/2 (Add duplicate node)",
                fun add_node_duplicate/0},
            {"API -> add_node/2 (max conn attempts made)",
                fun add_node_max_attempt/0},
            {"API -> add_node/4",
                fun add_node_with_cbs/0},
            {"API -> add_connect_callback",
                fun add_connect_callback/0},
            {"API -> remove_connect_callback",
                fun remove_connect_callback/0},
            {"API -> add_disconnect_callback",
                fun add_disconnect_callback/0},
            {"API -> remove_disconnect_callback",
                fun remove_disconnect_callback/0},
            {"API -> remove_node/1",
                fun remove_node/0},
            {"API -> node_state/1",
                fun node_state/0},
            {"node disconnect call disconnect callback",
                fun node_connect_disconnect/0},
            {"add already existing node, with same callbacks.",
                fun add_node_same_cbs/0},
            {"add already existing node, with diff callbacks.",
                fun add_node_diff_cbs/0},
            {"add hidden node test",
                fun add_hidden_node/0}
        ]
    }.

-define(TEST_NODE_NAME, 'test_node').

-define(TEST_NODE(Host),
    list_to_atom(atom_to_list(?TEST_NODE_NAME)++"@"++Host)
).

%%--------------------------------------------------------

hawk_nodes() ->
    ?assertEqual([], hawk:nodes()).

hawk_node_exists() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    ?assertEqual(false, hawk:node_exists(Node)).

add_node() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(10, fun() ->
            hawk:node_exists(Node)
        end)
    ),
    ?assertEqual([Node], hawk:connected_nodes()),
    ?assertEqual(ok, hawk:remove_node(Node)).

add_node_duplicate() ->
   {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    {error,{already_started,Pid}} = hawk:add_node(Node, Cookie),
    ?assertEqual(ok, hawk:remove_node(Node)).

add_node_max_attempt() ->
    ok = application:set_env(hawk, connection_retries, 2),
    ok = application:set_env(hawk, conn_retry_wait, 10),
    Node = list_to_atom("tests@some_fake_hostname"),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, fake_cookie),
    ?assertEqual([Node], hawk:nodes()),
    %% Now lets wait more than the configured allowed time ( check setup/0 )
    timer:sleep(1500),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ok = application:set_env(hawk, connection_retries, 600),
    ok = application:set_env(hawk, conn_retry_wait, 100).

add_node_with_cbs() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {ok,Pid} = hawk:add_node(Node, Cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertMatch(
        {ok, #{
            conn_cb_list := [{connected,ConnF}],
            conn_retry_wait := 1000,
            connected := true,
            cookie := cookie,
            disc_cb_list := [{disconnected,DisConnF}],
            node := TEST_NODE}
        },
        hawk:node_state(Node)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),
    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual([], check_node_table(Node)).

add_connect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual({ok,updated},
                 hawk:add_connect_callback(Node, {extra_conn_cb,fun() -> node_connected2(Node) end})
    ),
    ?assertEqual(ok, hawk:remove_node(Node)).

remove_connect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {ok,Pid} = hawk:add_node(Node, Cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),
    ?assertEqual({ok,updated}, hawk:remove_connect_callback(Node, connected)),
    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual([], check_node_table(Node)).

add_disconnect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual({ok,updated},
                 hawk:add_disconnect_callback(Node, {extra_conn_cb,fun() -> node_disconnected2(Node) end})
    ),
    ?assertEqual(ok, hawk:remove_node(Node)).

remove_disconnect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {ok,Pid} = hawk:add_node(Node, Cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),
    ?assertEqual({ok,updated}, hawk:remove_disconnect_callback(Node, disconnected)),
    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual([{Node,0}], check_node_table(Node)).

remove_node() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual({error,no_such_node}, hawk:remove_node(Node)),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),

    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual(false, hawk:node_exists(Node)).

node_state() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    {ok,NS} = hawk:node_state(Node),
    ?assertMatch({ok,#{conn_cb_list := [],
                       conn_retry_wait := 1000,
                       connected := true,
                       cookie := cookie,
                       disc_cb_list := [],
                       node := Node}}, hawk:node_state(Node)),
    ?assertEqual(ok, hawk:remove_node(Node)).

%%--------------------------------------------------------

node_connect_disconnect() ->
    % ?assertEqual([], hawk:nodes()),
    % ?assertEqual(false, hawk:node_exists(Node)),
    % ConnF = fun() -> node_connected(Node) end,
    % DisConnF = fun() -> node_disconnected(Node) end,
    % {ok,Pid} = hawk:add_node(Node, Cookie,
    %     [{connected, ConnF}],
    %     [{disconnected, DisConnF}]
    % ),
    % ?assertEqual([Node], hawk:nodes()),
    % ?assertEqual([{Node,0}], check_node_table(Node)),
    % ?assertEqual(
    %     {ok,Pid,[connected,disconnected]},
    %     keep_calling(100, fun() -> hawk:node_exists(Node) end)
    % ),


    %% Here kill the Node,

    %% check that disconnect callback ran...

    % ?assertEqual([{Node,0}], check_node_table(Node)),
    % ?assertEqual(ok, hawk:remove_node(Node)),
    % ?assertEqual([], check_node_table(Node)).

    % whereis(Node) ! {nodedown, Node},
    % slave:stop(Node),

    % timer:sleep(1000),
    % %% The disconnect callback should've removed the entry..
    % ?assertEqual([], check_node_table(Node)),
    % ?assertEqual([], hawk:nodes()),
    % ?assertEqual(false, hawk:node_exists(Node)),
    % ?assertEqual(ok, hawk:remove_node(Node)),

    % do_slave_start().

    ok.

add_node_same_cbs() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),

    %% add the same node again.
    {error,{already_started,Pid}} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),

    %% Do the same as the above 2 steps, also specify callbacks
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),

    %% test adding the same callbacks
    {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),

    ?assertEqual(ok, hawk:remove_node(Node)).

add_node_diff_cbs() ->
    {ok, Host} = inet:gethostname(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),

    %% add the same node again.
    {error,{already_started,Pid}} = hawk:add_node(Node, Cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),

    %% Do the same as the above 2 steps, also specify callbacks
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    ConnF2 = fun() -> node_connected2(Node) end,
    DisConnF2 = fun() -> node_disconnected2(Node) end,
    {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),

    %% test adding other callbacks
    {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
        [{connected, ConnF}, {connected2, ConnF2}],
        [{disconnected, DisConnF}, {disconnected2, DisConnF2}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected2,connected,disconnected2,disconnected]},
        keep_calling(100, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),
    ?assertEqual([{Node,0}], check_node_table2(Node)),

    ?assertEqual(ok, hawk:remove_node(Node)).

add_hidden_node() ->
    ok.

%%--------------------------------------------------------

setup() ->
    % Hostname = string:strip(os:cmd("hostname"), right, $\n ),
    % EtcHostname = os:cmd("cat /etc/hostname"),
    % ?assertEqual(
    %     Hostname,
    %     EtcHostname
    % ),
    {ok, Host} = inet:gethostname(),
    traceme(),
    node_table = ets:new(node_table, [public, named_table, set]),
    node_table2 = ets:new(node_table2, [public, named_table, set]),
    ok = application:load(hawk),
    %% Hawk has to at least, give a node +- 1000ms/1Sec chance to connect. ( 10 retry * 100 ms )
    ok = application:set_env(hawk, connection_retries, 10),
    ok = application:set_env(hawk, conn_retry_wait, 1000),
    ok = application:start(hawk),
    {ok, _} = make_distrib("hawk_node@"++Host, shortnames),
    do_slave_start().

cleanup(Slaves) ->
    {ok, Host} = inet:gethostname(),
    [ ok = slave:stop(SlaveNodeName) || SlaveNodeName <- Slaves ],
    true = ets:delete(node_table),
    true = ets:delete(node_table2),
    ok = net_kernel:stop(),
    ok = application:stop(hawk),
    ok = application:unload(hawk).

%%--------------------------------------------------------

do_slave_start() ->
    {ok, Host} = inet:gethostname(),
    {ok, SlaveName} = slave:start(Host, ?TEST_NODE_NAME),
    [SlaveName].

-spec make_distrib( NodeName::string()|atom(), NodeType::shortnames | longnames)
        -> {ok, ActualNodeName::atom} | {error, Reason::term()}.
make_distrib(NodeName, NodeType) when is_list(NodeName) ->
    make_distrib(erlang:list_to_atom(NodeName), NodeType);
make_distrib(NodeName, NodeType) ->
    case node() of
        'nonode@nohost' ->
            [] = os:cmd("epmd -daemon"),
            case net_kernel:start([NodeName, NodeType]) of
                {ok, _Pid} ->
                    % true = erlang:set_cookie(node(), cookie),
                    {ok, node()}
            end;
        CurrNode ->
            CurrNode
    end.

keep_calling(0, _) ->
    {error, no_answer};
keep_calling(Times, F) ->
    case F() of
        {error, connecting} ->
            timer:sleep(25),
            ?debugFmt("..... KEEP ~p CALLING ~p ......~n", [Times, nodes()]),
            keep_calling(Times-1, F);
        Else ->
            Else
    end.

node_connected(Node) ->
    % ?debugFmt("node_connected(~p) ->~n", [Node]),
    ets:insert(node_table, {Node, 0}).

node_connected2(Node) ->
    % ?debugFmt("node_connected2(~p) ->~n", [Node]),
    ets:insert(node_table2, {Node, 0}).

node_disconnected(Node) ->
    % ?debugFmt("node_disconnected(~p) ->~n", [Node]),
    ets:delete(node_table, Node).

node_disconnected2(Node) ->
    % ?debugFmt("node_disconnected2(~p) ->~n", [Node]),
    ets:delete(node_table2, Node).

check_node_table(Node) ->
    % ?debugFmt("check_node_table(~p) ->~n", [Node]),
    ets:lookup(node_table, Node).

check_node_table2(Node) ->
    % ?debugFmt("check_node_table2(~p) ->~n", [Node]),
    ets:lookup(node_table2, Node).

traceme() ->
    %dbg:tracer(),
    %dbg:p(all, call),
    %dbg:tpl(hawk_node, cx),
    % dbg:tpl(hawk_node_mon, cx),
    % dbg:tpl(net_kernel, connect_node, cx),
    ok.

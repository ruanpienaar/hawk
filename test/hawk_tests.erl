-module(hawk_tests).

-include_lib("eunit/include/eunit.hrl").

api_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"API -> nodes/0",
                fun hawk_nodes/0},
            {"API -> hawk_node_exists/1",
                fun hawk_node_exists/0},
            {"API -> add_node/2",
                fun add_node/0},
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
                fun node_state/0}
        ]
    }.

%%--------------------------------------------------------

hawk_nodes() ->
    ?assertEqual([], hawk:nodes()).

hawk_node_exists() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual(false, hawk:node_exists(Node)).

add_node() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual(ok, hawk:remove_node(Node)).

add_node_with_cbs() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {ok,Pid} = hawk:add_node(Node, cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),
    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual([], check_node_table(Node)).

add_connect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual({ok,updated},
                 hawk:add_connect_callback(Node, {extra_conn_cb,fun() -> node_connected2(Node) end})
    ),
    ?assertEqual(ok, hawk:remove_node(Node)).

remove_connect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {ok,Pid} = hawk:add_node(Node, cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),
    ?assertEqual({ok,updated}, hawk:remove_connect_callback(Node, connected)),
    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual([], check_node_table(Node)).

add_disconnect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual({ok,updated},
                 hawk:add_disconnect_callback(Node, {extra_conn_cb,fun() -> node_disconnected2(Node) end})
    ),
    ?assertEqual(ok, hawk:remove_node(Node)).

remove_disconnect_callback() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {ok,Pid} = hawk:add_node(Node, cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual([{Node,0}], check_node_table(Node)),
    ?assertEqual({ok,updated}, hawk:remove_disconnect_callback(Node, disconnected)),
    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual([{Node,0}], check_node_table(Node)).

remove_node() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),

    ?assertEqual({error,no_such_node}, hawk:remove_node(Node)),

    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),

    ?assertEqual(ok, hawk:remove_node(Node)),
    ?assertEqual(false, hawk:node_exists(Node)).

node_state() ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    {ok,Pid} = hawk:add_node(Node, cookie),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ?assertEqual({ok,#{conn_cb_list => [],
                       connected => true,
                       cookie => cookie,
                       disc_cb_list => [],
                       node => tests@mbp}}, hawk:node_state(Node)),
    ?assertEqual(ok, hawk:remove_node(Node)).

%%--------------------------------------------------------

setup() ->
    node_table = ets:new(node_table, [public, named_table, set]),
    node_table2 = ets:new(node_table2, [public, named_table, set]),
    ok = application:start(hawk),
    {ok, Host} = inet:gethostname(),
    make_distrib("tests@"++Host, shortnames),
    slave:start(Host, test1),
    ok.

cleanup(ok) ->
    ets:delete(node_table),
    ets:delete(node_table2),
    stop_distrib(),
    ok = application:stop(hawk).

%%--------------------------------------------------------

node_connect_disconnect_test() ->
    node_table = ets:new(node_table, [public, named_table, set]),
    node_table2 = ets:new(node_table2, [public, named_table, set]),
    ok = application:start(hawk),
    {ok, Host} = inet:gethostname(),
    make_distrib("tests@"++Host, shortnames),
    slave:start(Host, test1),

    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    ?assertEqual([], hawk:nodes()),
    ?assertEqual(false, hawk:node_exists(Node)),
    ConnF = fun() -> node_connected(Node) end,
    DisConnF = fun() -> node_disconnected(Node) end,
    {ok,Pid} = hawk:add_node(Node, cookie,
        [{connected, ConnF}],
        [{disconnected, DisConnF}]
    ),
    ?assertEqual([Node], hawk:nodes()),
    ?assertEqual(
        {ok,Pid,[connected,disconnected]},
        keep_calling(10, fun() -> hawk:node_exists(Node) end)
    ),
    ok.


    %% Here kill the node,

    %% check that disconnect callback ran...

    % ?assertEqual([{Node,0}], check_node_table(Node)),
    % ?assertEqual(ok, hawk:remove_node(Node)),
    % ?assertEqual([], check_node_table(Node)).

%%--------------------------------------------------------

-spec make_distrib( NodeName::string()|atom(), NodeType::shortnames | longnames) ->
    {ok, ActualNodeName::atom} | {error, Reason::term()}.
make_distrib(NodeName, NodeType) when is_list(NodeName) ->
    make_distrib(erlang:list_to_atom(NodeName), NodeType);
make_distrib(NodeName, NodeType) ->
    case node() of
        'nonode@nohost' ->
            [] = os:cmd("epmd -daemon"),
            case net_kernel:start([NodeName, NodeType]) of
                {ok, _Pid} -> node()
            end;
        CurrNode ->
            CurrNode
    end.

stop_distrib()->
    net_kernel:stop().

keep_calling(0, _) ->
    {error, no_answer};
keep_calling(Times, F) ->
    case F() of
        {error, connecting} ->
            timer:sleep(50),
            keep_calling(Times-1, F);
        Else ->
            Else
    end.

node_connected(Node) ->
    ets:insert(node_table, {Node, 0}).

node_connected2(Node) ->
    ets:insert(node_table2, {Node, 0}).

node_disconnected(Node) ->
    ets:delete(node_table, Node).

node_disconnected2(Node) ->
    ets:delete(node_table2, Node).

check_node_table(Node) ->
    ets:lookup(node_table, Node).

-module(hawk_integration_tests).

% -include_lib("kernel/include/inet.hrl").
% -include_lib("eunit/include/eunit.hrl").

% api_test_() ->
%     {foreach,
%         fun setup/0,
%         fun cleanup/1,
%         [
%             {"API -> nodes/0",
%                 fun hawk_nodes/0},
%             {"API -> hawk_node_exists/1",
%                 fun hawk_node_exists/0},
%             {"API -> add_node/2",
%                 fun add_node/0},
%             {"API -> add_node/2 (Add duplicate node)",
%                 fun add_node_duplicate/0},
%             {"API -> add_node/2 (max conn attempts made)",
%                 fun add_node_max_attempt/0},
%             {"API -> add_node/4",
%                 fun add_node_with_cbs/0},
%             {"API -> add_connect_callback",
%                 fun add_connect_callback/0},
%             {"API -> remove_connect_callback",
%                 fun remove_connect_callback/0},
%             {"API -> add_disconnect_callback",
%                 fun add_disconnect_callback/0},
%             {"API -> remove_disconnect_callback",
%                 fun remove_disconnect_callback/0},
%             {"API -> remove_node/1",
%                 fun remove_node/0},
%             {"API -> node_state/1",
%                 fun node_state/0},
%             {"node disconnect call disconnect callback",
%                 fun node_connect_disconnect/0},
%             {"add already existing node, with same callbacks.",
%                 fun add_node_same_cbs/0},
%             {"add already existing node, with diff callbacks.",
%                 fun add_node_diff_cbs/0},
%             {"add hidden node test",
%                 fun add_hidden_node/0}
%         ]
%     }.

% -define(TEST_NODE_NAME, 'test_node').

% -define(TEST_NODE(Host),
%     list_to_atom(atom_to_list(?TEST_NODE_NAME)++"@"++Host)
% ).

% %%--------------------------------------------------------

% hawk_nodes() ->
%     ?assertEqual([], hawk:nodes()).

% hawk_node_exists() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     ?assertEqual(false, hawk:node_exists(Node)).

% add_node() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(10, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual([Node], hawk:connected_nodes()),
%     ?assertEqual(ok, hawk:remove_node(Node)).

% add_node_duplicate() ->
%    {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     {error,{already_started,Pid}} = hawk:add_node(Node, Cookie),
%     ?assertEqual(ok, hawk:remove_node(Node)).

% add_node_max_attempt() ->
%     ok = application:set_env(hawk, connection_retries, 2),
%     ok = application:set_env(hawk, conn_retry_wait, 10),
%     Node = list_to_atom("tests@some_fake_hostname"),
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,_Pid} = hawk:add_node(Node, fake_cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     %% Now lets wait more than the configured allowed time ( check setup/0 )
%     timer:sleep(1500),
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     ok = application:set_env(hawk, connection_retries, 600),
%     ok = application:set_env(hawk, conn_retry_wait, 100).

% add_node_with_cbs() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     ConnF = fun() -> node_connected(Node) end,
%     DisConnF = fun() -> node_disconnected(Node) end,
%     {ok,Pid} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}],
%         [{disconnected, DisConnF}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertMatch(
%         {ok, #{
%             conn_cb_list := [{connected,ConnF}],
%             conn_retry_wait := 1000,
%             connected := true,
%             cookie := cookie,
%             disc_cb_list := [{disconnected,DisConnF}],
%             node := Node}
%         },
%         hawk:node_state(Node)
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),
%     ?assertEqual(ok, hawk:remove_node(Node)),
%     ?assertEqual([], check_node_table(Node)).

% add_connect_callback() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual({ok,updated},
%                  hawk:add_connect_callback(Node, {extra_conn_cb,fun() -> node_connected2(Node) end})
%     ),
%     ?assertEqual(ok, hawk:remove_node(Node)).

% remove_connect_callback() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     ConnF = fun() -> node_connected(Node) end,
%     DisConnF = fun() -> node_disconnected(Node) end,
%     {ok,Pid} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}],
%         [{disconnected, DisConnF}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),
%     ?assertEqual({ok,updated}, hawk:remove_connect_callback(Node, connected)),
%     ?assertEqual(ok, hawk:remove_node(Node)),
%     ?assertEqual([], check_node_table(Node)).

% add_disconnect_callback() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual({ok,updated},
%                  hawk:add_disconnect_callback(Node, {extra_conn_cb,fun() -> node_disconnected2(Node) end})
%     ),
%     ?assertEqual(ok, hawk:remove_node(Node)).

% remove_disconnect_callback() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     ConnF = fun() -> node_connected(Node) end,
%     DisConnF = fun() -> node_disconnected(Node) end,
%     {ok,Pid} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}],
%         [{disconnected, DisConnF}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),
%     ?assertEqual({ok,updated}, hawk:remove_disconnect_callback(Node, disconnected)),
%     ?assertEqual(ok, hawk:remove_node(Node)),
%     ?assertEqual([{Node,0}], check_node_table(Node)).

% remove_node() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual({error,no_such_node}, hawk:remove_node(Node)),
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),

%     ?assertEqual(ok, hawk:remove_node(Node)),
%     ?assertEqual(false, hawk:node_exists(Node)).

% node_state() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertMatch({ok,#{conn_cb_list := [],
%                        conn_retry_wait := 1000,
%                        connected := true,
%                        cookie := cookie,
%                        disc_cb_list := [],
%                        node := Node}}, hawk:node_state(Node)),
%     ?assertEqual(ok, hawk:remove_node(Node)).

% %%--------------------------------------------------------

% node_connect_disconnect() ->
%     {ok, Host} = inet:gethostname(),
%     % start a extra slave
%     [ExtraSlaveNodename] = unit_testing:slaves_setup([{Host, 'extra_slave'}]),

%     % connect to the extra slave
%     Node = ExtraSlaveNodename,
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     ConnF = fun() -> node_connected(Node) end,
%     DisConnF = fun() -> node_disconnected(Node) end,
%     {ok,Pid} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}],
%         [{disconnected, DisConnF}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertMatch(
%         {ok, #{
%             conn_cb_list := [{connected,ConnF}],
%             conn_retry_wait := 1000,
%             connected := true,
%             cookie := cookie,
%             disc_cb_list := [{disconnected,DisConnF}],
%             node := Node}
%         },
%         hawk:node_state(Node)
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),


%     % check that callback was called

%     % stop the extra slave
%     true = unit_testing:cleanup_slaves([ExtraSlaveNodename]),

%     % check that discon callback was called

%     % check that trying to reconnect

%     % Remove the extra slave node
%     ?assertEqual(ok, hawk:remove_node(Node)),
%     ?assertEqual([], check_node_table(Node)),

%     ok.

% add_node_same_cbs() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),

%     %% add the same node again.
%     {error,{already_started,Pid}} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),

%     %% Do the same as the above 2 steps, also specify callbacks
%     ConnF = fun() -> node_connected(Node) end,
%     DisConnF = fun() -> node_disconnected(Node) end,
%     {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}],
%         [{disconnected, DisConnF}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),

%     %% test adding the same callbacks
%     {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}],
%         [{disconnected, DisConnF}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),

%     ?assertEqual(ok, hawk:remove_node(Node)).

% add_node_diff_cbs() ->
%     {ok, Host} = inet:gethostname(),
%     Node = ?TEST_NODE(Host),
%     Cookie = cookie,
%     ?assertEqual([], hawk:nodes()),
%     ?assertEqual(false, hawk:node_exists(Node)),
%     {ok,Pid} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),

%     %% add the same node again.
%     {error,{already_started,Pid}} = hawk:add_node(Node, Cookie),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),

%     %% Do the same as the above 2 steps, also specify callbacks
%     ConnF = fun() -> node_connected(Node) end,
%     DisConnF = fun() -> node_disconnected(Node) end,
%     ConnF2 = fun() -> node_connected2(Node) end,
%     DisConnF2 = fun() -> node_disconnected2(Node) end,
%     {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}],
%         [{disconnected, DisConnF}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),

%     %% test adding other callbacks
%     {error,{already_started,Pid}} = hawk:add_node(Node, Cookie,
%         [{connected, ConnF}, {connected2, ConnF2}],
%         [{disconnected, DisConnF}, {disconnected2, DisConnF2}]
%     ),
%     ?assertEqual([Node], hawk:nodes()),
%     ?assertEqual(
%         {ok,Pid,[connected2,connected,disconnected2,disconnected]},
%         unit_testing:wait_for_next_value(100, fun() -> hawk:node_exists(Node) end, {error, connecting})
%     ),
%     ?assertEqual([{Node,0}], check_node_table(Node)),
%     ?assertEqual([{Node,0}], check_node_table2(Node)),

%     ?assertEqual(ok, hawk:remove_node(Node)).

% add_hidden_node() ->
%     ok.

% %%--------------------------------------------------------

% setup() ->
%     node_table = ets:new(node_table, [public, named_table, set]),
%     node_table2 = ets:new(node_table2, [public, named_table, set]),
%     ok = application:load(hawk),
%     %% Hawk has to at least, give a node +- 1000ms/1Sec chance to connect. ( 10 retry * 100 ms )
%     ok = application:set_env(hawk, connection_retries, 10),
%     ok = application:set_env(hawk, conn_retry_wait, 1000),
%     ok = application:start(hawk),
%     {ok, Host} = inet:gethostname(),
%     {ok, _} = unit_testing:start_distrib(list_to_atom("hawk_node@"++Host), shortnames, 750),
%     _Slaves = unit_testing:slaves_setup([{Host, ?TEST_NODE_NAME}]).

% cleanup(Slaves) ->
%     true = ets:delete(node_table),
%     true = ets:delete(node_table2),
%     true = unit_testing:cleanup_slaves(Slaves),
%     ok = unit_testing:stop_distrib(),
%     timer:sleep(25),
%     ok = application:stop(hawk),
%     ok = application:unload(hawk).

% %%--------------------------------------------------------

% node_connected(Node) ->
%     % ?debugFmt("node_connected(~p) ->~n", [Node]),
%     ets:insert(node_table, {Node, 0}).

% node_connected2(Node) ->
%     % ?debugFmt("node_connected2(~p) ->~n", [Node]),
%     ets:insert(node_table2, {Node, 0}).

% node_disconnected(Node) ->
%     % ?debugFmt("node_disconnected(~p) ->~n", [Node]),
%     ets:delete(node_table, Node).

% node_disconnected2(Node) ->
%     % ?debugFmt("node_disconnected2(~p) ->~n", [Node]),
%     ets:delete(node_table2, Node).

% check_node_table(Node) ->
%     % ?debugFmt("check_node_table(~p) ->~n", [Node]),
%     ets:lookup(node_table, Node).

% check_node_table2(Node) ->
%     % ?debugFmt("check_node_table2(~p) ->~n", [Node]),
%     ets:lookup(node_table2, Node).
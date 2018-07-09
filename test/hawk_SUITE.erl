-module(hawk_SUITE).
-export([
    all/0,
    suite/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    group/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).
% Success Tests
-export([
    node_exists/1,
    node_exists_while_connect/1,
    add_node_2/1,
    add_node_2_while_connecting/1,
    add_node_4/1,
    add_node_4_while_connecting/1,
    add_connect_callback/1,
    add_connect_callback_while_connect/1,
    add_disconnect_callback/1,
    add_disconnect_callback_while_connect/1,
    remove_connect_callback/1,
    remove_connect_callback_while_connect/1,
    remove_disconnect_callback/1,
    remove_disconnect_callback_while_connect/1,
    remove_node/1,
    connected_nodes/1,
    callback_names/1
]).
% Failure tests
-export([
    add_node_conn_attempts_exceeded_limit/1,
    add_node_conn_callback_duplicate/1,
    add_node_disconn_callback_duplicate/1,
    node_conn_callback_fails/1,
    node_disconn_callback_fails/1
]).
-include_lib("common_test/include/ct.hrl").

-ifdef(SYSTEM_TIME).
-define(SYSTEM_TIME_FUNC, erlang:now()).
-else.
-define(SYSTEM_TIME_FUNC,
    erlang:system_time()
).
-endif.

% Returns a list of all test cases and groups in the suite. (Mandatory)
all() ->
    [{group, success_test_group}
    ,{group, failure_test_group}
    ].

all_success() ->
    [node_exists
    ,node_exists_while_connect
    ,add_node_2
    ,add_node_2_while_connecting
    ,add_node_4
    ,add_node_4_while_connecting
    ,add_connect_callback
    ,add_connect_callback_while_connect
    ,add_disconnect_callback
    ,add_disconnect_callback_while_connect
    ,remove_connect_callback
    ,remove_connect_callback_while_connect
    ,remove_disconnect_callback
    ,remove_disconnect_callback_while_connect
    ,remove_node
    ,connected_nodes
    ,callback_names
    ].

all_failure() ->
    [add_node_conn_attempts_exceeded_limit
     ,add_node_conn_callback_duplicate
     ,add_node_disconn_callback_duplicate
     ,node_conn_callback_fails
     ,node_disconn_callback_fails
    ].

% Information function used to return properties for the suite. (Optional)
suite() ->
    [{timetrap, {minutes, 10}} % wait for 10, better than the default 30min wait.
    ].

% For declaring test case groups. (Optional)
groups() ->
    [
        {success_test_group, [shuffle,{repeat,10}], all_success()}
       ,{failure_test_group, [shuffle,{repeat,10}], all_failure()}
    ].

% Suite level configuration function, executed before the first test case. (Optional)
init_per_suite(Config) ->
    % {ok, _} = dbg:tracer(),
    % {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(hawk_sup, cx),
    % {ok, _} = dbg:tpl(hawk_node, cx),
    % {ok, _} = dbg:tpl(hawk, cx),
    % {ok, _} = dbg:tpl(hawk, add_node, cx),
    % {ok, _} = dbg:tpl(hawk, remove_node, cx),
    % {ok, _} = dbg:tpl(hawk_node, connected_callback, cx),
    % {ok, _} = dbg:tpl(hawk_node, do_wait, cx),
    % {ok, _} = dbg:tpl(hawk_node, loop, cx),
    % {ok, _} = dbg:tpl(hawk_node, do_terminate, cx),
    % {ok, _} = dbg:tpl(hawk_nodes_sup, delete_child, cx),
    % {ok, _} = dbg:tpl(hawk_app, cx),
    % {ok, _} = dbg:tpl(hawk_sup, cx),
    % {ok, _} = dbg:tpl(application_master, cx),
    % {ok, _} = dbg:tpl(unit_testing, wait_for_match, cx),
    {ok, _} = erlang_testing:start_distrib(new_node_name(), shortnames),
    ok = application:start(hawk),
    Config.

% Suite level configuration function, executed after the last test case. (Optional)
end_per_suite(_Config) ->
    ok = application:stop(hawk),
    erlang_testing:stop_distrib().

% Information function used to return properties for a test case group. (Optional)
group(_GroupName) ->
    [].

% Configuration function for a group, executed before the first test case. (Optional)
init_per_group(_GroupName, Config) ->
    Config.

% Configuration function for a group, executed after the last test case. (Optional)
end_per_group(_GroupName, _Config) ->
    ok.

% Configuration function for a testcase, executed before each test case. (Optional)

init_per_testcase(_TestCase, Config) ->
    ok = application:set_env(hawk, connection_retries, 600),
    ok = application:set_env(hawk, conn_retry_wait, 100),
    node_table = ets:new(node_table, [public, named_table, set]),
    {ok, Host} = inet:gethostname(),

    [] = os:cmd("epmd -daemon"),
    N1 = new_node_name(Host),
    os:cmd("ps aux | grep -v grep | grep beam"),


    % N1 = new_node_name(),
    N2 = new_node_name(),
    N3 = new_node_name(),
    N4 = new_node_name(),
    N5 = new_node_name(),
    Slaves = erlang_testing:slaves_setup([
        {Host, N1}
       ,{Host, N2}
       ,{Host, N3}
       ,{Host, N4}
       ,{Host, N5}
    ]),

    ct:log("init_per_testcase Slaves -> ~p~n", [Slaves]),
    [{slaves, Slaves} | Config].

% Configuration function for a testcase, executed after each test case. (Optional)
end_per_testcase(_TestCase, Config) ->
    true = ets:delete(node_table),
    lists:foreach(fun(N) ->
        ct:log("Remove node ~p~n", [N]),
        hawk:remove_node(N)
    end, hawk:nodes()),
    [] = hawk:nodes(),
    {slaves, Slaves} = lists:keyfind(slaves, 1, Config),
    true = erlang_testing:cleanup_slaves(Slaves).


%---------------------------------------------------------------------------------------------
% Notes:
% node will be considered as a "wrong/incorrect/fake" nodename.
% A working started node will be entered into Config as {running_node, Node}.

node_exists(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie()),
    F = fun() ->
        case hawk:node_state(Slave) of
            {ok, {_, #{ connected := C} = _M}} ->
                C;
            _ ->
                false
        end
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    {ok,_Pid, []} = hawk:node_exists(Slave),
    {ok, {_Pid,#{conn_cb_list := [],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [],
                 node := Slave}}} = hawk:node_state(Slave).

node_exists_while_connect(_Config) ->
    Node = new_node_name(),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node, cookie),
    [Node] = hawk:nodes(),
    {error, connecting} = hawk:node_exists(Node).

add_node_2(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie()),
    [Slave] = hawk:nodes(),
    F = fun() ->
        {error, connecting} =/= hawk:node_state(Slave)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    {error, {already_started, _Pid}} =
        hawk:add_node(Slave, erlang:get_cookie()),
    [Slave] = hawk:nodes().

add_node_2_while_connecting(_Config) ->
    Node = new_node_name(),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node, cookie),
    [Node] = hawk:nodes(),
    {error, connecting} = hawk:add_node(Node, cookie).

add_node_4(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    DCb = fun() -> node_action(Slave, disconnect) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], [{disconn_cb, DCb}]),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    [{Slave, connected, _}] = node_tbl(Slave),
    CCb2 = fun() -> node_action(Slave, connected2) end,
    DCb2 = fun() -> node_action(Slave, disconnect2) end,
    {error, {already_started, _Pid}} =
        hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb2, CCb2}], [{disconn_cb2, DCb2}]),
    F2 = fun() ->
        match_node_action(Slave, connected2)
    end,
    ok = unit_testing:wait_for_match(100, F2, true).

add_node_4_while_connecting(_Config) ->
    Node = new_node_name(),
    [] = hawk:nodes(),
    CCb = fun() -> node_action(Node, connected) end,
    DCb = fun() -> node_action(Node, disconnect) end,
    {ok, _} = hawk:add_node(Node, cookie, [{conn_cb, CCb}], [{disconn_cb, DCb}]),
    [Node] = hawk:nodes(),
    {error, connecting} = hawk:add_node(Node, cookie).

add_connect_callback(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    DCb = fun() -> node_action(Slave, disconnect) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    [{Slave, connected, _}] = node_tbl(Slave),
    % Add connect callback
    CCb2 = fun() -> node_action(Slave, connected2) end,
    {ok,{_, updated}} = hawk:add_connect_callback(Slave, {conn_cb2, CCb2}),
    F2 = fun() ->
        match_node_action(Slave, connected2)
    end,
    ok = unit_testing:wait_for_match(100, F2, true),
    {ok, {_Pid, #{conn_cb_list := [{conn_cb2,CCb2}, {conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb, DCb}],
                 node := Slave}}} = hawk:node_state(Slave).

add_connect_callback_while_connect(_Config) ->
    Node = new_node_name(),
    {ok, _} = hawk:add_node(Node, cookie),
    {ok, {Pid, #{conn_cb_list := [],
                 conn_retry_wait := 100,
                 connected := false,
                 connection_retries := _,
                 cookie := cookie,
                 disc_cb_list := [],
                 node := Node}}} = hawk:node_state(Node),
    % Add connect callback
    CCb2 = fun() -> node_action(Node, connected2_extra) end,
    {ok,{_, updated}} = hawk:add_connect_callback(Node, {conn_cb2_extra, CCb2}),
    {ok, {Pid, #{conn_cb_list := [{conn_cb2_extra,CCb2}],
                 conn_retry_wait := 100,
                 connected := false,
                 connection_retries := _,
                 cookie := cookie,
                 disc_cb_list := [],
                 node := Node}}} = hawk:node_state(Node).

add_disconnect_callback(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    DCb = fun() -> node_action(Slave, disconnect) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    [{Slave, connected, _}] = node_tbl(Slave),
    {ok, {Pid, #{conn_cb_list := [{conn_cb, CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb,DCb}],
                 node := Slave}}} = hawk:node_state(Slave),
    % Add disconnect callback
    DCb2 = fun() -> node_action(Slave, disconnect2) end,
    {ok,{_, updated}} = hawk:add_disconnect_callback(Slave, {disconn_cb2, DCb2}),
    {ok, {Pid, #{conn_cb_list := [{conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb2, DCb2}, {disconn_cb, DCb}],
                 node := Slave}}} = hawk:node_state(Slave),
    % Stop the remote note, to test the disconnect callback
    ok = slave:stop(Slave),
    F2 = fun() ->
        match_node_action(Slave, disconnect) ==
        match_node_action(Slave, disconnect2)
    end,
    % hawk_node is trying to reconnect.
    % wait at least 60 s for now, so that the reconnect attempts finish.
    ok = unit_testing:wait_for_match(2401, F2, true).

add_disconnect_callback_while_connect(_Config) ->
    Node = new_node_name(),
    [] = hawk:nodes(),
    CCb = fun() -> node_action(Node, connected) end,
    DCb = fun() -> node_action(Node, disconnect) end,
    {ok, _} = hawk:add_node(Node, cookie, [{conn_cb, CCb}], [{disconn_cb, DCb}]),
    [Node] = hawk:nodes(),
    % Add disconnect callback
    DCb2 = fun() -> node_action(Node, disconnect2) end,
    {ok,{_, updated}} = hawk:add_disconnect_callback(Node, {disconn_cb2, DCb2}),
    {ok, {_Pid, #{conn_cb_list := [{conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := false,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb2, DCb2}, {disconn_cb, DCb}],
                 node := Node}}} = hawk:node_state(Node).


remove_connect_callback(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    DCb = fun() -> node_action(Slave, disconnect) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    [{Slave, connected, _}] = node_tbl(Slave),
    % remote connect callback
    {ok, {_, updated}} = hawk:remove_connect_callback(Slave, conn_cb),
    {ok, {_Pid, #{conn_cb_list := [],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb, DCb}],
                 node := Slave}}} = hawk:node_state(Slave).

remove_connect_callback_while_connect(_Config) ->
    ok.

remove_disconnect_callback(_Config) ->
    ok.

remove_disconnect_callback_while_connect(_Config) ->
    ok.

remove_node(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], []),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    ok = hawk:remove_node(Slave),
    [] = hawk:nodes().

connected_nodes(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie()),
    [Slave] = hawk:nodes(),
    [Slave] = hawk:connected_nodes().

callback_names(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    DCb = fun() -> node_action(Slave, disconnect) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    [{Slave, connected, _}] = node_tbl(Slave),
    {ok, _Pid, [conn_cb,disconn_cb]} = hawk:callback_names(Slave).

add_node_conn_attempts_exceeded_limit(_Config) ->
    % Set the the retry limit very low.
    ok = application:set_env(hawk, connection_retries, 1),
    ok = application:set_env(hawk, conn_retry_wait, 10),
    % Non existing node
    {ok, _} = hawk:add_node(foo_node, bar_cookie, [], []),
    timer:sleep(100),
    [] = hawk:nodes().

add_node_conn_callback_duplicate(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    DCb = fun() -> node_action(Slave, disconnect) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    [{Slave, connected, _}] = node_tbl(Slave),
    % Add connect callback
    CCb2 = fun() -> node_action(Slave, connected2) end,
    {ok,{_, updated}} = hawk:add_connect_callback(Slave, {conn_cb2, CCb2}),
    F2 = fun() ->
        match_node_action(Slave, connected2)
    end,
    ok = unit_testing:wait_for_match(100, F2, true),
    {ok, {_Pid, #{conn_cb_list := [{conn_cb2,CCb2}, {conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb, DCb}],
                 node := Slave}}} = hawk:node_state(Slave),
    % Duplicate
    {ok, {_Pid, duplicate}} = hawk:add_connect_callback(Slave, {conn_cb2, CCb2}).

add_node_disconn_callback_duplicate(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    CCb = fun() -> node_action(Slave, connected) end,
    DCb = fun() -> node_action(Slave, disconnect) end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true),
    [{Slave, connected, _}] = node_tbl(Slave),
    % Add disconnect callback
    DCb2 = fun() -> node_action(Slave, disconnect2) end,
    {ok,{_, updated}} = hawk:add_disconnect_callback(Slave, {disconn_cb2, DCb2}),
    {ok, {_Pid, #{conn_cb_list := [{conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb2, DCb2}, {disconn_cb, DCb}],
                 node := Slave}}} = hawk:node_state(Slave),
    % Duplicate
    {ok, {_Pid, duplicate}} = hawk:add_disconnect_callback(Slave, {disconn_cb2, DCb2}).

node_conn_callback_fails(_Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, _Config),
    CCb = fun() ->
        node_action(Slave, connected),
        timer:sleep(1000)
        % throw({test_throw_in_hawk_SUITE})
    end,
    {ok, _} = hawk:add_node(Slave, erlang:get_cookie(), [{conn_cb, CCb}], []),
    F = fun() ->
        match_node_action(Slave, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true).

node_disconn_callback_fails(_Config) ->
    ok.

%% --------------------------------------------------------------------------------

node_tbl(Node) ->
    ets:lookup(node_table, Node).

node_action(Node, Action) ->
    ets:insert(node_table, {Node, Action, ?SYSTEM_TIME_FUNC}).

match_node_action(Node, Action) ->
    case ets:lookup(node_table, Node) of
        [{Node, Action, _Time}] ->
            true;
        _ ->
            false
    end.

new_node_name() ->
    list_to_atom(erlang:ref_to_list(make_ref()) -- "#Ref<>...").

new_node_name(Host) when is_list(Host) ->
    list_to_atom(erlang:ref_to_list(make_ref()) -- "#Ref<>..." ++ "@"++Host).

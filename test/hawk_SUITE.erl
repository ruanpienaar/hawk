-module(hawk_SUITE).
-include_lib("eunit/include/eunit.hrl").
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
        {success_test_group, 
            [shuffle,{repeat,10}], 
            all_success()}
       ,{failure_test_group, 
            [shuffle,{repeat,10}], 
            all_failure()}
    ].

% Suite level configuration function, executed before the first test case. (Optional)
init_per_suite(Config) ->
    os:cmd("ps aux | grep node1@localhost | grep -v grep | awk '{print$2}' | xargs kill"),
    os:cmd("ps aux | grep node2@localhost | grep -v grep | awk '{print$2}' | xargs kill"),
    %{ok, _} = dbg:tracer(),
    %{ok, _} = dbg:p(all, call),
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
    % {ok, _} = dbg:tpl(net_kernel, cx),
    % {ok, _} = dbg:tpl(auth, cx),
    %{ok, _} = dbg:tpl(ct_N1, cx),
    {ok, _} = erlang_testing:start_distrib('hawk_SUITE@localhost', shortnames),
    true = auth:set_cookie(hawk),
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
    N1 = 'node1@localhost',
    N2 = 'node2@localhost',
    ct:pal("Starting nodes ~p ~p\n", [N1, N2]),
    [Erl] = string:tokens(os:cmd("which erl"), "\n"),
    [] = os:cmd(Erl++" -sname "++atom_to_list(N1)++" -setcookie cookie -noshell -noinput -detached"),
    [] = os:cmd(Erl++" -sname "++atom_to_list(N2)++" -setcookie cookie -noshell -noinput -hidden -detached"),
    ct:pal("Stage 4\n", []),
    [{nodes, [N1, N2]}| Config].

% Configuration function for a testcase, executed after each test case. (Optional)
end_per_testcase(TestCase, Config) ->
    % ct:pal("TestCase ~p\n", [TestCase]),
    % io:format("TestCase ~p\n", [TestCase]),
    do_end_per_testcase(Config).

do_end_per_testcase(Config) ->
    true = ets:delete(node_table),
    lists:foreach(fun(N) ->
        ct:pal("Remove node ~p~n", [N]),
        hawk:remove_node(N)
    end, hawk:nodes()),
    [] = hawk:nodes(),
    % {N1s, N1s} = lists:keyfind(N1s, 1, Config),
    % {nodes, Nodes} = lists:keyfind(nodes, 1, Config),
    % erlang_testing:ct_cleanup_N1s(N1s).
    
    os:cmd("ps aux | grep node1@localhost | grep -v grep | awk '{print$2}' | xargs kill -9"),
    ok = unit_testing:wait_for_match(100, fun() ->
        os:cmd("ps aux | grep node1@localhost | grep -v grep")
    end, [], 100),

    os:cmd("ps aux | grep node2@localhost | grep -v grep | awk '{print$2}' | xargs kill -9"),
    ok = unit_testing:wait_for_match(100, fun() ->
        os:cmd("ps aux | grep node2@localhost | grep -v grep")
    end, [], 100).

%---------------------------------------------------------------------------------------------
% Notes:
% node will be considered as a "wrong/incorrect/fake" nodename.
% A working started node will be entered into Config as {running_node, Node}.

node_exists(Config) ->
    ct:pal("TEST node_exists wait stage 1\n", []),
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
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
    ok = unit_testing:wait_for_match(50, F, true, 100),
    ct:pal("TEST node_exists wait stage 3\n", []),
    {ok,_Pid, []} = hawk:node_exists(N1),
    ct:pal("TEST node_exists wait stage 4\n", []),
    {ok, {_Pid,#{conn_cb_list := [],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [],
                 node := N1}}} = hawk:node_state(N1).

node_exists_while_connect(_Config) ->
    Node = new_node_name(),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node, cookie),
    [Node] = hawk:nodes(),
    {ok,_,[]} = hawk:node_exists(Node).

add_node_2(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    {ok, _} = hawk:add_node(N1, cookie),
    [N1] = hawk:nodes(),
    F = fun() ->
        {error, connecting} =/= hawk:node_state(N1)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    {error, {already_started, _Pid}} =
        hawk:add_node(N1, cookie),
    [N1] = hawk:nodes().

add_node_2_while_connecting(_Config) ->
    Node = new_node_name(),
    [] = hawk:nodes(),
    {ok, _} = hawk:add_node(Node, cookie),
    [Node] = hawk:nodes(),
    {error,{already_started,_}} = hawk:add_node(Node, cookie).

add_node_4(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    DCb = fun() -> node_action(N1, disconnect) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], [{disconn_cb, DCb}]),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    [{{N1, connected}, _}] = node_tbl(N1, connected),
    CCb2 = fun() -> node_action(N1, connected2) end,
    DCb2 = fun() -> node_action(N1, disconnect2) end,
    {error, {already_started, _Pid}} =
        hawk:add_node(N1, cookie, [{conn_cb2, CCb2}], [{disconn_cb2, DCb2}]),
    F2 = fun() ->
        match_node_action(N1, connected2)
    end,
    ok = unit_testing:wait_for_match(100, F2, true, 100).

add_node_4_while_connecting(_Config) ->
    Node = new_node_name(),
    [] = hawk:nodes(),
    CCb = fun() -> node_action(Node, connected) end,
    DCb = fun() -> node_action(Node, disconnect) end,
    ct:pal("1\n\n", []),
    {ok, _} = hawk:add_node(Node, cookie, [{conn_cb, CCb}], [{disconn_cb, DCb}]),
    [Node] = hawk:nodes(),
    ct:pal("2\n\n", []),
    {error,{already_started,_}} = hawk:add_node(Node, cookie).

add_connect_callback(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    DCb = fun() -> node_action(N1, disconnect) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    [{{N1, connected}, _}] = node_tbl(N1, connected),
    % Add connect callback
    CCb2 = fun() -> node_action(N1, connected2) end,
    {ok,{_, updated}} = hawk:add_connect_callback(N1, {conn_cb2, CCb2}),
    F2 = fun() ->
        match_node_action(N1, connected2)
    end,
    ok = unit_testing:wait_for_match(100, F2, true, 100),
    {ok, {_Pid, #{conn_cb_list := [{conn_cb2,CCb2}, {conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb, DCb}],
                 node := N1}}} = hawk:node_state(N1).

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
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    DCb = fun() -> node_action(N1, disconnect) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    ct:pal("connected action matched !\n", []),
    [{{N1, connected}, _}] = node_tbl(N1, connected),
    {ok, {Pid, #{conn_cb_list := [{conn_cb, CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb,DCb}],
                 node := N1}}} = hawk:node_state(N1),
    % Add disconnect callback
    DCb2 = fun() -> node_action(N1, disconnect2) end,
    {ok,{_, updated}} = hawk:add_disconnect_callback(N1, {disconn_cb2, DCb2}),
    {ok, {Pid, #{conn_cb_list := [{conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb2, DCb2}, {disconn_cb, DCb}],
                 node := N1}}} = hawk:node_state(N1),
    % Stop the remote note, to test the disconnect callback
    % ok = rpc:call(N1, erlang , halt, 0),
    NetSupPid = rpc:call(N1, erlang, whereis, [net_sup]),
    true = rpc:call(N1, erlang, exit, [NetSupPid, kill]),
    % [] = os:cmd("ps aux | grep node1@localhost | grep -v grep | awk '{print$2}' | xargs kill"),
    ok = unit_testing:wait_for_match(50, fun() ->
        os:cmd(io_lib:format("ps aux | grep ~s | grep -v grep", [N1]))
    end, [], 100),
    F2 = fun() ->
        timer:sleep(20),
        AA = match_node_action(N1, disconnect),
        BB = match_node_action(N1, disconnect2),
        % ct:pal("AA ~p\n", [AA]),
        ct:pal("BB ~p\n", [BB]),
        AA == BB
    end,
    % hawk_node is trying to reconnect.
    % wait at least 60 s for now, so that the reconnect attempts finish.
    ok = unit_testing:wait_for_match(50, F2, true, 100),
    ct:pal("both disconnected actions matched !\n", []),
    ok.

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
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    DCb = fun() -> node_action(N1, disconnect) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    [{{N1, connected}, _}] = node_tbl(N1, connected),
    % remote connect callback
    {ok, {_, updated}} = hawk:remove_connect_callback(N1, conn_cb),
    {ok, {_Pid, #{conn_cb_list := [],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb, DCb}],
                 node := N1}}} = hawk:node_state(N1).

remove_connect_callback_while_connect(_Config) ->
    ok.

remove_disconnect_callback(_Config) ->
    ok.

remove_disconnect_callback_while_connect(_Config) ->
    ok.

remove_node(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], []),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    ok = hawk:remove_node(N1),
    [] = hawk:nodes().

connected_nodes(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    {ok, _} = hawk:add_node(N1, cookie),
    [N1] = hawk:nodes(),
    [N1] = hawk:connected_nodes().

callback_names(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    DCb = fun() -> node_action(N1, disconnect) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    [{{N1, connected}, _}] = node_tbl(N1, connected),
    {ok, _Pid, [conn_cb,disconn_cb]} = hawk:callback_names(N1).

add_node_conn_attempts_exceeded_limit(_Config) ->
    % Set the the retry limit very low.
    ok = application:set_env(hawk, connection_retries, 1),
    ok = application:set_env(hawk, conn_retry_wait, 10),
    % Non existing node
    {ok, _} = hawk:add_node(foo_node, bar_cookie, [], []),
    timer:sleep(100),
    [] = hawk:nodes().

add_node_conn_callback_duplicate(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    DCb = fun() -> node_action(N1, disconnect) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    [{{N1, connected}, _}] = node_tbl(N1, connected),
    % Add connect callback
    CCb2 = fun() -> node_action(N1, connected2) end,
    {ok,{_, updated}} = hawk:add_connect_callback(N1, {conn_cb2, CCb2}),
    F2 = fun() ->
        match_node_action(N1, connected2)
    end,
    ok = unit_testing:wait_for_match(100, F2, true, 100),
    {ok, {_Pid, #{conn_cb_list := [{conn_cb2,CCb2}, {conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb, DCb}],
                 node := N1}}} = hawk:node_state(N1),
    % Duplicate
    {ok, {_Pid, duplicate}} = hawk:add_connect_callback(N1, {conn_cb2, CCb2}).

add_node_disconn_callback_duplicate(Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, Config),
    CCb = fun() -> node_action(N1, connected) end,
    DCb = fun() -> node_action(N1, disconnect) end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], [{disconn_cb,DCb}]),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100),
    [{{N1, connected}, _}] = node_tbl(N1, connected),
    % Add disconnect callback
    DCb2 = fun() -> node_action(N1, disconnect2) end,
    {ok,{_, updated}} = hawk:add_disconnect_callback(N1, {disconn_cb2, DCb2}),
    {ok, {_Pid, #{conn_cb_list := [{conn_cb,CCb}],
                 conn_retry_wait := 100,
                 connected := true,
                 connection_retries := _,
                 cookie := _Cookie,
                 disc_cb_list := [{disconn_cb2, DCb2}, {disconn_cb, DCb}],
                 node := N1}}} = hawk:node_state(N1),
    % Duplicate
    {ok, {_Pid, duplicate}} = hawk:add_disconnect_callback(N1, {disconn_cb2, DCb2}).

node_conn_callback_fails(_Config) ->
    {nodes, [N1, _N2]} = lists:keyfind(nodes, 1, _Config),
    CCb = fun() ->
        node_action(N1, connected),
        timer:sleep(1000)
        % throw({test_throw_in_hawk_SUITE})
    end,
    {ok, _} = hawk:add_node(N1, cookie, [{conn_cb, CCb}], []),
    F = fun() ->
        match_node_action(N1, connected)
    end,
    ok = unit_testing:wait_for_match(100, F, true, 100).

node_disconn_callback_fails(_Config) ->
    ok.

%% --------------------------------------------------------------------------------

node_tbl(Node, Action) ->
    ets:lookup(node_table, {Node, Action}).

node_action(Node, Action) ->
    ct:pal("NODE ~p ACTION ~p\n", [Node, Action]),
    ets:insert(node_table, {{Node, Action}, ?SYSTEM_TIME_FUNC}).

match_node_action(Node, Action) ->
    case ets:lookup(node_table, {Node, Action}) of
        [] ->
            false;
        ActionsHistory ->
            ct:pal("ActionsHistory was ~p\n", [ActionsHistory]),
            lists:keyfind({Node, Action}, 1, ActionsHistory) /= false
    end.

new_node_name() ->
    list_to_atom(erlang:ref_to_list(make_ref()) -- "#Ref<>...").

new_node_name(Host) when is_list(Host) ->
    list_to_atom(erlang:ref_to_list(make_ref()) -- "#Ref<>..." ++ "@"++Host).

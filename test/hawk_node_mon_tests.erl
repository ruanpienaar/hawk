-module(hawk_node_mon_tests).

-include_lib("eunit/include/eunit.hrl").

hawk_node_mon_unit_test_() ->
    unit_testing:setup(
        % Setup
        fun() ->
            ok
        end,
        % Cleanup
        fun(_) ->
            ok
        end,
        % Tests
        [{"hawk_node_mon:start_link/0",
            ?_assert(unit_testing:try_test_fun(fun start_link/0))}
        %  {"hawk_node_mon:loop/0",
        %     ?_assert(unit_testing:try_test_fun(fun loop/0))}
        ],
        % Mocks
        [
          % Mocks done in tests
        ],
        true
    ).

start_link() ->
    {ok, Pid} = hawk_node_mon:start_link(),
    true = erlang:unlink(Pid),
    ?assert(is_pid(whereis(hawk_node_mon))),
    ?assertEqual(
        [
            {current_function, {hawk_node_mon, loop, 0}},
            {initial_call, {proc_lib, init_p, 5}},
            {status, waiting},
            {message_queue_len, 0},
            {links, []},
            {trap_exit, true},
            {error_handler, error_handler},
            {priority, normal},
            {suspending,[]}
        ],
        erlang:process_info(
            whereis(hawk_node_mon),
            [
                current_function,
                initial_call,
                status,
                message_queue_len,
                links,
                trap_exit,
                error_handler,
                priority,
                suspending
            ]
        )
    ),
    ?assert(erlang:exit(whereis(hawk_node_mon), kill)),
    unit_testing:wait_for_match(100, fun() -> is_pid(whereis(hawk_node_mon)) end, false),
    ?assertNot(is_pid(whereis(hawk_node_mon))).

% loop() ->
%     % Start it up
%     Pid = spawn_link(fun hawk_node_mon:loop/0),
%     Pid ! {add_node, node()},

%     % nodeup Scenario 1 - Node is NOT registered
%     Pid ! {nodeup, node()},
%     % prints out error_msg

%     % nodeup Scenario 2 - Node is registered...
%     true = erlang:register(hawk_nodes_sup:id(node()), self()),
%     Pid ! {nodeup, node()},
%     receive
%         {nodeup, Node} when Node == node() ->
%             ok
%     after
%         1000 ->
%             throw(fail)
%     end.
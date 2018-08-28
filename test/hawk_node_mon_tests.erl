-module(hawk_node_mon_tests).
-include_lib("eunit/include/eunit.hrl").

hawk_node_mon_unit_test_() ->
    % {setup,
    %  % Setup Fixture
    %  fun() -> 
    %      xxx
    %  end,
    %  % Cleanup Fixture
    %  fun(xxx) ->
    %      ok
    %  end,
    %  % List of tests
    %  [
    %    % Example test
    %    {"hawk_node_mon:start_link/0",
    %         ?_assert(unit_testing:try_test_fun(fun start_link/0))}
    %  ]
    % }.
    unit_testing:setup(
        % Setup
        fun() ->
            %?debugFmt("SELF SETUP ~p\n", [self()]),
            ok
        end,
        % Cleanup
        fun(_) ->
            % ?debugFmt("SELF CLEANUP ~p\n", [self()]),
            ok
        end,
        % Tests
        [{"hawk_node_mon:start_link/0",
            ?_assert(unit_testing:try_test_fun(fun start_link/0))},
         {"hawk_node_mon:loop/0",
            ?_assert(unit_testing:try_test_fun(fun loop/0))}

        ],
        % Mocks
        [
          % Mocks done in tests  
        ],
        true
    ).

start_link() ->
    % ?debugFmt("SELF TEST ~p\n", [self()]),
    {ok, Pid} = hawk_node_mon:start_link(),
    true = erlang:unlink(Pid),
    % ?assertMatch(
    %     {ok, _},
        
    % ).
    ok.

loop() ->
    % Start it up
    Pid = spawn_link(fun hawk_node_mon:loop/0),
    Pid ! {add_node, node()},

    % nodeup Scenario 1 - Node is NOT registered
    Pid ! {nodeup, node()},
    % prints out error_msg

    % nodeup Scenario 2 - Node is registered...
    erlang:register(hawk_nodes_sup:id(node()), self()),
    Pid ! {nodeup, node()},
    receive
        {nodeup, Node} when Node == node() ->
            ok
    after
        1000 ->
            throw(fail)
    end.
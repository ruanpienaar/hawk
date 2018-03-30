-module(hawk_node_mon_tests).
-include_lib("eunit/include/eunit.hrl").

hawk_node_mon_unit_test_() ->
    {setup,
     % Setup Fixture
     fun() -> 
         xxx
     end,
     % Cleanup Fixture
     fun(xxx) ->
         ok
     end,
     % List of tests
     [
       % Example test
       {"hawk_node_mon:func1/0",
            ?_assert(unit_testing:try_test_fun(fun func1/0))}
     ]
    }.

func1() ->
    ?assert(
        is_list(hawk_node_mon:module_info())
    ).

-module(hawk_node_tests).
-include_lib("eunit/include/eunit.hrl").

hawk_node_unit_test_() ->
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
       {"hawk_node:func1/0",
            ?_assert(unit_testing:try_test_fun(fun func1/0))}
     ]
    }.

func1() ->
    ?assert(
        is_list(hawk_node:module_info())
    ).

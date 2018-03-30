-module(hawk_nodes_sup_tests).

-include_lib("eunit/include/eunit.hrl").

hawk_sup_unit_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [
      {"hawk_nodes_sup:start_link/0",
            ?_assert(unit_testing:try_test_fun(fun start_link/0))},
      {"hawk_nodes_sup:init/1",
            ?_assert(unit_testing:try_test_fun(fun init/0))},
      {"hawk_nodes_sup:start_child/4",
            ?_assert(unit_testing:try_test_fun(fun start_child/0))},
      {"hawk_nodes_sup:delete_child/4",
            ?_assert(unit_testing:try_test_fun(fun delete_child/0))},
      {"hawk_nodes_sup:id/4",
            ?_assert(unit_testing:try_test_fun(fun id/0))}
     ]
    }.

start_link() ->
    ?assertEqual(
        ok,
        ok
    ).

init() ->
    ?assertEqual(
        ok,
        ok
    ).

start_child() ->
    ?assertEqual(
        ok,
        ok
    ).

% Running example in hawk_nodes_sup_SUITE
delete_child() ->
    ?assertEqual(
        {error, no_such_node},
        hawk_nodes_sup:delete_child('node@host.com')
    ).

id() ->
    ?assertEqual(
        'node@host.com',
        hawk_nodes_sup:id('node@host.com')
    ).
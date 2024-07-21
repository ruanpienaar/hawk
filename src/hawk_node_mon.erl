-module(hawk_node_mon).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/0,
    do_start_link/0,
    add_node/2
]).

-ifdef(TEST).
-export([
    loop/0
]).
-endif.

start_link() ->
    proc_lib:start_link(?MODULE, do_start_link, []).

do_start_link() ->
    _ = process_flag(trap_exit, true),
    true = erlang:register(?MODULE, self()),
    ?MODULE = ets:new(?MODULE, [named_table, private, ordered_set]),
    ok = net_kernel:monitor_nodes(true),
    ok = proc_lib:init_ack({ok, self()}),
    loop().

add_node(Node, NodePid) ->
    whereis(?MODULE) ! {add_node, Node, NodePid, self()},
    receive
        ok ->
            ok
    after
        5000 ->
            timeout
    end.

% is_known_node(Node) ->
%     ok.

loop() ->
    receive
    % TODO: rename to monitor_node
        {add_node, Node, NodePid, ReqPid} ->
            ?LOG_NOTICE(#{going_to_monitor_node => Node}),
            true = erlang:monitor_node(Node, true),
            true = erlang:link(NodePid),
            ReqPid ! ok,
            loop();
        {nodeup, Node} ->
            % true = ets:insert(?MODULE,
            case whereis(hawk_nodes_sup:id(Node)) of
                undefined -> % Something else was adding nodes, possibly supress logging
                    ?LOG_ERROR(#{
                        hawk_nodes_sup_unknown_nodeup => Node,
                        all_nodes => supervisor:which_children(hawk_nodes_sup)
                    });
                Pid ->
                    ?LOG_NOTICE(#{
                        nodeup => Node,
                        all_nodes => supervisor:which_children(hawk_nodes_sup)
                    }),
                    Pid ! {nodeup, Node},
                    ok
            end,
            loop();
        {nodedown, Node} ->
            ok = case whereis(hawk_nodes_sup:id(Node)) of
                undefined ->  % Something else was removing nodes, possibly supress logging
                    ?LOG_ERROR(#{
                        hawk_nodes_sup_unknown_nodedown => Node,
                        all_nodes => supervisor:which_children(hawk_nodes_sup)
                    });
                Pid ->
                    ?LOG_NOTICE(#{
                        nodedown => Node,
                        all_nodes => supervisor:which_children(hawk_nodes_sup)
                    }),
                    Pid ! {nodedown, Node},
                    ok
            end,
            loop();
        Msg ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => Msg}),
            loop()
    end.
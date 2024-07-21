-module(hawk_node_mon2).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
    start_link/0,
    add_node/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/4,
    terminate/3
]).

start_link() ->
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        {},
        []
    ).

add_node(Node, NodePid) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Node, NodePid}).

init({}) ->
    _ = process_flag(trap_exit, true),
    ok = net_kernel:monitor_nodes(
        true,
        #{node_type => all, nodedown_reason => true}
    ),
    {ok, #{}}.

handle_call({add_node, Node, NodePid}, _From, Data) ->
    ?LOG_NOTICE(#{going_to_monitor_node => Node}),
    true = erlang:monitor_node(Node, true),
    true = erlang:link(NodePid),
    {reply, ok, Data}.

handle_cast(undefined, Data) ->
    {noreply, Data}.

% TODO: Keep track of nodes in Data?
handle_info({'EXIT', _NodePid, shutdown}, Data) ->
    {noreply, Data};
handle_info({nodeup, Node}, Data) ->
    ok = handle_node_up(Node),
    {noreply, Data};
handle_info({nodeup, Node, #{node_type := _Type}}, Data) ->
    ok = handle_node_up(Node),
    {noreply, Data};
handle_info({nodedown, Node}, Data) ->
    ok = handle_node_down(Node),
    {noreply, Data};
handle_info({nodedown, Node, #{node_type := _Type, nodedown_reason := _Reason}}, Data) ->
    % ?LOG_NOTICE(#{info => Info}),
    ok = handle_node_down(Node),
    {noreply, Data}.

handle_node_up(Node) ->
    case whereis(hawk_nodes_sup:id(Node)) of
        undefined -> % Something else was adding nodes, possibly supress logging
            ?LOG_ERROR(#{
                unknown_nodeup => Node,
                all_nodes => supervisor:which_children(hawk_nodes_sup)
            });
        Pid ->
            ?LOG_NOTICE(#{
                nodeup => Node,
                all_nodes => supervisor:which_children(hawk_nodes_sup)
            }),
            Pid ! {nodeup, Node},
            ok
    end.

handle_node_down(Node) ->
    case whereis(hawk_nodes_sup:id(Node)) of
        undefined ->  % Something else was removing nodes, possibly supress logging
            ?LOG_ERROR(#{
                unknown_nodedown => Node,
                all_nodes => supervisor:which_children(hawk_nodes_sup)
            });
        Pid ->
            ?LOG_NOTICE(#{
                nodedown => Node,
                all_nodes => supervisor:which_children(hawk_nodes_sup)
            }),
            Pid ! {nodedown, Node},
            ok
    end.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(Reason, State, Data) ->
    ?LOG_WARNING(#{
        terminate_reason => Reason,
        terminate_state => State,
        terminate_data => Data
    }).
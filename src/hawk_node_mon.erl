-module(hawk_node_mon).
-export([
    start_link/0,
    add_node/2
]).

start_link() ->
    {ok, proc_lib:spawn_link(fun() ->
        ok = net_kernel:monitor_nodes(true),
        true = erlang:register(?MODULE, self()),
        loop([])
    end)}.

add_node(Node, Cookie) ->
    whereis(?MODULE) ! {add_node, Node, Cookie},
    ok.

loop(Nodes) ->
    receive
        M={add_node, Node, _Cookie} ->
            % io:format("A : ~p~n", [M]),
            true = erlang:monitor_node(Node, true),
            % ok = net_kernel:monitor_nodes(false),
            % N = application:get_env(kernel, sync_nodes_optional, []),
            % ok = application:set_env(kernel, sync_nodes_optional, [Node|N]),
            % ok = net_kernel:monitor_nodes(true),
            loop([Node|Nodes]);
        M={nodeup, Node} ->
            % io:format("A : ~p~n", [M]),
            whereis(Node) ! {nodeup, Node},
            loop(Nodes);
        M={nodedown, Node} ->
            % io:format("A : ~p~n", [M]),
            whereis(Node) ! {nodedown, Node},
            loop(Nodes);
        A ->
            % io:format("A : ~p~n", [A]),
            loop(Nodes)
    end.

% -module(hawk_node_mon).
% -export([start_link/0,
%          connect/2
% ]).

% -behaviour(gen_server).
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% start_link() ->
%     gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

% %% @private
% init({}) ->
%     {ok, undefined}.

% %% @private
% handle_call({add_node, Node, Cookie}, _From, State) ->
%     connect(Node, Cookie),
%     {reply, ok, State};
% handle_call(_Request, _From, State) ->
%     {reply, {error, unknown_call}, State}.

% %% @private
% handle_cast(_Msg, State) ->
%     {noreply, State}.

% %% @private
% handle_info(Info={nodedown,Node}, State) ->
%     timer:apply_after(5000, ?MODULE, connect, [Node, hawk]),
%     io:format("Info : ~p~n", [Info]),
%     {noreply, State};
% handle_info(Info, State) ->
%     io:format("Info : ~p~n", [Info]),
%     {noreply, State}.

% %% @private
% terminate(_Reason, _State) ->
%     ok.

% %% @private
% code_change(_OldVsn, State, _Extra) ->
%     {ok, State}.

% connect(Node, Cookie) ->
%     io:format("Connect...~p~n", [Node]),
%     true = erlang:monitor_node(Node, true),
%     ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect_node(Node))). %% left this here, to get {nodeup to work..
%     % ok = net_kernel:monitor_nodes(false),
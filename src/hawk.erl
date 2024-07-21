-module(hawk).

-compile({no_auto_import,[nodes/0]}).
-export([
    nodes/0,
    node_exists/1,
    add_node/2,
    add_node/4,
    add_connect_callback/2,
    add_disconnect_callback/2,
    remove_connect_callback/2,
    remove_disconnect_callback/2,
    remove_node/1,
    node_state/1,
    callback_names/1,
    connected_nodes/0
]).

% -ifdef(TEST).
% -export([
%     call/2,
%     call/3
% ]).
% -endif.

% -define(R, hawk_req).

% -type hawk_node_call_errors_return() :: {'error', 'connecting'} |
%                                         {error, no_such_node} |
%                                         'timeout'.
% -type hawk_node_call_return() :: hawk_node_call_errors_return() |
%                                  {'ok', any()}.
% -type callback_names_return() :: {ok, pid(), list(callback_name())} |
%                                  hawk_node_call_errors_return().
% -type callbacks() :: maybe_improper_list() |
%                      list(callback_type()).
% -type callback_name() :: any().
% -type callback() :: fun().
% -type callback_type() :: {callback_name(), callback()}.
% -type hawk_node_cmd() ::
%     callbacks |
%     state |
%     {add_connect_callback, callback_type()} |
%     {add_disconnect_callback, callback_type()} |
%     {remove_connect_callback, callback_name()} |
%     {remove_disconnect_callback, callback_name()}.

% -export_type([
%     callbacks/0,
%     callback_type/0
% ]).

-spec nodes() -> list(term()).
nodes() ->
    % [ begin
    %     [$h,$a,$w,$k,$_,$n,$o,$d,$e,$_|Rest] = atom_to_list(N),
    %     list_to_atom(Rest)
    %   end || {N,_,worker,[hawk_node]}
    %   <- supervisor:which_children(hawk_nodes_sup) ].
    hawk_nodes_sup:nodes().

% -spec node_exists(atom()) -> callback_names_return().
node_exists(Node) ->
    hawk_node:callback_names(Node).

% -spec add_node(atom(), atom())
%         -> hawk_nodes_sup:start_child_return() |
%            hawk_node_call_errors_return().
add_node(Node, Cookie) ->
    add_node(Node, Cookie, [], []).

% -spec add_node(atom(), atom(), callbacks(), callbacks())
%         -> hawk_nodes_sup:start_child_return() |
%            hawk_node_call_errors_return().
add_node(Node, Cookie, ConnectedCallback, DisconnectedCallback)
        when is_atom(Node) andalso
             is_atom(Cookie) andalso
             is_list(ConnectedCallback) andalso
             is_list(DisconnectedCallback) ->
    hawk_nodes_sup:start_child(Node, Cookie, ConnectedCallback, DisconnectedCallback).

% -spec add_connect_callback(atom(), callback_type())
%         -> hawk_node_call_return().
% PS Appends to the existing connect callbacks.
add_connect_callback(Node, {Name, ConnectCallback})
        when is_function(ConnectCallback) ->
    % call(Node, {add_connect_callback, {Name, ConnectCallback}}).
    hawk_node:add_connect_callback(Node, {Name, ConnectCallback}).

% -spec add_disconnect_callback(atom(), callback_type())
%         -> hawk_node_call_return().
% PS Appends to the existing connect callbacks.
add_disconnect_callback(Node, {Name, DisconnectCallback})
        when is_function(DisconnectCallback) ->
    hawk_node:add_disconnect_callback(Node, {Name, DisconnectCallback}).

% -spec remove_connect_callback(atom(), callback_name())
%         -> hawk_node_call_return().
remove_connect_callback(Node, Name) ->
    hawk_node:remove_connect_callback(Node, Name).

% -spec remove_disconnect_callback(atom(), callback_name())
%         -> hawk_node_call_return().
remove_disconnect_callback(Node, Name) ->
    hawk_node:remove_disconnect_callback(Node, Name).

% -spec remove_node(atom())
%         -> hawk_nodes_sup:delete_child_return().
remove_node(Node) ->
    hawk_nodes_sup:delete_child(Node).

% -spec node_state(atom())
%         -> hawk_node_call_return().
% TODO: Move to hawk_node.erl
node_state(Node) ->
    sys:get_state(hawk_nodes_sup:id(Node)).

% -spec callback_names(atom())
%         -> callback_names_return().
callback_names(Node) ->
    case node_exists(Node) of
        {ok, {ConCBS, DisCBS}} ->
            lists:map(fun({Name,_}) ->
                Name
            end, lists:append(ConCBS, DisCBS));
        Error ->
            Error
    end.

    % case call(Node, callbacks) of
    %     {ok, {Pid, {ConCBS, DisCBS}}} when is_list(ConCBS) andalso is_list(DisCBS) ->
    %         Callbacknames = lists:map(fun({Name,_}) ->
    %             Name
    %         end, lists:flatten([ConCBS,DisCBS])),
    %         {ok, Pid, Callbacknames};
    %     {error, no_such_node} ->
    %         {error, no_such_node};
    %     {error, connecting} ->
    %         {error, connecting};
    %     timeout ->
    %         timeout
    % end.

% TODO: test hidden nodes.
% -spec connected_nodes()
%         -> list(atom()).
connected_nodes() ->
    lists:filter(fun(Node) ->
        {_Pid, State} = node_state(Node),
        State =/= {error, connecting}
    end, hawk:nodes()).

%%-------------------------------------------------------------------------------------------

% -spec call(atom(), hawk_node_cmd())
%         -> hawk_node_call_return().
% call(Node, Cmd) ->
%     call(Node, Cmd, 1000).

% hawk:call(atom(),'callbacks' |
%                  'state' |
%                  {'add_connect_callback',{_,fun()}} |
%                  {'add_disconnect_callback',{_,fun()}} |
%                  {'remove_connect_callback',_} |
%                  {'remove_disconnect_callback',_},
%           1000) ->
%     'timeout' |
%     {'error','connecting'} |
%     {'ok',_}

% -type hawk_node_call_errors_return() :: {'error', 'connecting'} | 'timeout'.
% -type hawk_node_call_return() :: hawk_node_call_errors_return() | {'ok', term()}.

% -spec call(atom(), hawk_node_cmd(), pos_integer())
%         -> hawk_node_call_return().
% call(Node, Cmd, Timeout) ->
%     Name = hawk_nodes_sup:id(Node),
%     case whereis(Name) of
%         undefined ->
%             {error, no_such_node};
%         Pid ->
%             Pid ! {call, Cmd, self()},
%             receive
%                 {response, connecting} ->
%                     {error, connecting};
%                 {response, Response} ->
%                     {ok, {Pid, Response}}
%             after
%                 Timeout ->
%                     timeout
%             end
%     end.
-module(hawk_tcp_dist).
-export([listen/1,
         accept/1,
         accept_connection/5,
         setup/5,
         close/1,
         select/1,
         is_node_name/1
        ]).

-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").
-include_lib("kernel/include/net_address.hrl").

listen(Name) ->
    inet_tcp_dist:listen(Name).

accept(Listen) ->
    inet_tcp_dist:accept(Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    inet_tcp_dist:accept_connection( AcceptPid, Socket, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    HSData = #hs_data{kernel_pid = self(), other_node = Node, this_node = MyNode, this_flags = 0,f_send = fun inet_tcp:send/2, f_recv = fun inet_tcp:recv/3, f_setopts_pre_nodeup = fun setopts_pre_nodeup/1, f_setopts_post_nodeup = fun setopts_post_nodeup/1, f_getll = fun inet:getll/1, mf_tick = fun tick/1, mf_getstat = fun inet_tcp_dist:getstat/1, request_type =Type},
    spawn_opt(fun () -> do_setup(HSData, SetupTime) end, [link, {priority, max}]).

close(Socket) ->
    inet_tcp_dist:close(Socket).
select(Node) ->
    is_node_name(Node).
is_node_name(Node) ->
    inet_tcp_dist:is_node_name(Node).
do_setup(HSData = #hs_data{other_node = Node}, SetupTime) ->
    case lists:splitwith(fun (C) -> C =/= $@ end, atom_to_list(Node)) of
        {"", _} ->
            error_logger:error_msg("** Nodename ~p illegal **~n", [Node]),
            ?shutdown(Node);
        {_, ""} ->
            error_logger:error_msg("** Nodename ~p illegal, no '@' character **~n", [Node]),
            ?shutdown(Node);
        {Name, [$@ | Address]} ->
            do_setup_get_addr(HSData, Name, Address, SetupTime)
    end.

do_setup_get_addr(HSData, Name, Address, SetupTime) ->
    case inet:getaddr(Address, inet) of
        {ok, Ip} ->
            do_setup_port_timer(HSData, Name, Address, Ip, SetupTime);
        _Other ->
            ?shutdown(HSData#hs_data.other_node)
    end.

do_setup_port_timer(HSData, Name, Address, Ip, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    case erl_epmd:port_please(Name, Ip) of
        {port, TcpPort, Version} ->
            dist_util:reset_timer(Timer),
            NetAddress = #net_address{address = {Ip, TcpPort}, host = Address, protocol = tcp,family = inet},
            AddressFun = fun (_, _) -> NetAddress end,
            do_setup_connect(HSData#hs_data{timer = Timer,other_version = Version,f_address = AddressFun},Ip, TcpPort);
        _Other ->
            ?shutdown(HSData#hs_data.other_node)
    end.

do_setup_connect(HSData, Ip, TcpPort) ->
    case inet_tcp:connect(Ip, TcpPort, [{active, false}, {packet, 2}]) of
        {ok, Socket} ->
            dist_util:handshake_we_started(HSData#hs_data{socket = Socket});
        _Other ->
            ?shutdown(HSData#hs_data.other_node)
    end.

setopts_pre_nodeup(Socket) ->
    inet:setopts(Socket,
                 [{active, false},{packet, 4},{nodelay, nodelay()}]).

setopts_post_nodeup(Socket) ->
    inet:setopts(Socket,
                 [{active, true},{packet, 4},{nodelay, nodelay()},{deliver, port}]).

nodelay() ->
    application:get_env(kernel, dist_nodelay) =/= {ok, false}.
tick(Socket) ->
    case inet_tcp:send(Socket, [], [force]) of
        {error, closed} ->
            self() ! {tcp_closed, Socket},
            {error, closed};
        R ->
            R
    end.
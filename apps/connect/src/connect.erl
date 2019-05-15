%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%% Makes the initial connection of the calling process to the provided master
%%% Starts the application(s) that are responsible for all other actions.
%%% @end
%%% Created : 27 Mar 2018 by Alex Misch <alex@alex-Lenovo>

-module(connect).

-export([start_link/1, start_link/3, main/0, main/3]).

%% Number of attempts to make to connect to the server.
-define(ATTEMPTS, 5).
    
start_link(Port) ->
    application:set_env(master, default_port, Port),
    Pid = spawn_link(?MODULE, main, []),
    {ok, Pid}.

start_link(Server_IP, Server_Port, Port) ->
    Pid = spawn_link(?MODULE, main, [Server_IP, Server_Port, Port]),
    {ok, Pid}.

main() ->
    io:format("Starting master app~n"),
    supervisor:start_child(master_delay_start, []),
    io:format("~p~n", [application:ensure_all_started(interface)]).

main(Server_IP, Server_Port, Port) ->
    Socket = case gen_udp:open(Port, [{active, false}, {mode, binary}]) of
		 {error, _Inuse} ->
		     {ok, Socket} = gen_udp:open(0, [{active, false},
						     {mode, binary}]),
		     Socket;
		 {ok, Socket} ->
		     Socket
	     end,

    {ok, Master_Sctp_Port} = application:get_env(connect, sctp_port),

    {ok, Sctp_Port} = get_available_sctp(),
    io:format("Using SCTP Port: ~p~n", [Sctp_Port]),

    case connect(Socket, Server_IP, Server_Port, Sctp_Port) of

	{master, {Server_IP2, Server_Port, {backup}}} ->	
	    application:set_env(backup, server, {Server_IP2, Master_Sctp_Port}),
	    io:format("Setting server to ~p:~p at backup and file.~n", [Server_IP2, Master_Sctp_Port]),
	    application:set_env(backup, sctp, Sctp_Port),
	    application:set_env(backup, default_port, Server_Port),
	    application:set_env(master, default_port, Server_Port),
	    supervisor:start_child(backup_delay_start, []),

	    application:set_env(file, server, {Server_IP2, Master_Sctp_Port}),
	    application:set_env(file, sctp, Sctp_Port),
	    application:set_env(file, port, Port),
	    supervisor:start_child(file_delay_start, []);

	{master, {Server_IP2, Server_Port, {interface}}} ->
	    application:set_env(interface, server, {Server_IP2, Master_Sctp_Port}),
	    io:format("Setting server to ~p:~p at interface.~n", [Server_IP2, Master_Sctp_Port]),
	    application:set_env(interface, sctp, Sctp_Port),
	    application:start(interface);

	{master, {Server_IP2, _Port2, {file_server, BIP, BPort}}} ->
	    application:set_env(file, server, {Server_IP2, Master_Sctp_Port}),
	    application:set_env(file, backup, {BIP, BPort}),
	    application:set_env(file, sctp, Sctp_Port),
	    application:set_env(file, port, Port),
	    io:format("Setting server to ~p:~p at file.~n", [Server_IP2, Master_Sctp_Port]),
	    supervisor:start_child(file_delay_start, []);

	{error, Reason} ->
	    io:format("Error contacting master server: ~p~n", [Reason]);
	Reason ->
	    io:format("This should not happen: ~p~n", [Reason])
    end,
    gen_udp:close(Socket).

connect(Socket, Server_IP, Server_Port, Sctp_Port) ->
    connect(Socket, Server_IP, Server_Port, Sctp_Port, ?ATTEMPTS).

connect(_Socket, Server_IP, Server_Port, _Sctp_Port, 0) ->
    io:format("Error connecting to ~p:~p.~n", [Server_IP, Server_Port]),
    {error, timeout};
connect(Socket, Server_IP, Server_Port, Sctp_Port, Attempts) ->
    Node = list_to_binary(atom_to_list(node())),
    Packet = list_to_binary([<<0>>, <<Sctp_Port:16>>, Node]),
    ok = gen_udp:send(Socket, Server_IP, Server_Port, Packet),
    case gen_udp:recv(Socket, 0, 5000) of
	{error, timeout} ->
	    connect(Socket, Server_IP, Server_Port, Sctp_Port, Attempts - 1);
	{ok, {IP, Port, Response}} ->
	    Role = parse_packet(Response),
	    {master, {IP, Port, Role}};
	{error, Reason} ->
	    io:format("Error: ~p~n", [Reason]),
	    {error, Reason}
    end.

%% TODO: Parses a packet from the server.
%% Returns the designated role of the new server as an atom.
parse_packet(<<1, Node_Name/binary>>) ->
    Master_Node = list_to_atom(binary_to_list(Node_Name)),
    application:set_env(backup, master_node, Master_Node),
    {backup};
parse_packet(<<2>>) ->
    {interface};
parse_packet(<<3, BIP1:8, BIP2:8, BIP3:8, BIP4:8, BPort:8>>) ->
    BIP = {BIP1, BIP2, BIP3, BIP4},
    {file_server, BIP, BPort};
parse_packet(_Other) ->
    {error, not_found}.

%% Test purposes only.
%% parse_packet(<<0>>) ->
  %%   {backup}.

%% local_ip_v4() ->
%%     {ok, Addrs} = inet:getifaddrs(),
%%     [Next | Rest] = [Addr || {_, Opts} <- Addrs,
%% 			     {addr, Addr} <- Opts,
%% 			     size(Addr) == 4,
%% 			     Addr =/= {127, 0, 0, 1}],
%%     {ok, Next, Rest}.

%% get_node_name(Port) ->
%%     {ok, IP, _Other_IP} = local_ip_v4(),
%%     Node_Name = list_to_atom([Port, "@", tuple_to_list(IP)]),
%%     io:format("Node_Name: ~p~n", [Node_Name]),	    
%%     {ok, Node_Name}.


%% Not an elegant way to get a random free port, but it works.
get_available_sctp() ->
    {ok, Socket} = gen_sctp:open([{recbuf, 65000}]),
    {ok, {_IP, Port}} = inet:sockname(Socket),
    gen_sctp:close(Socket),
    {ok, Port}.

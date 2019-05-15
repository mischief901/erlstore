%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(gen_update).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add_master/1, get_updates/1, get_files/0,
	 update/2, lookup/1, delete/1, insert_file/2, add_backup/1,
	get_servers/0]).

-define(SERVER, ?MODULE).

-include("include/server_record.hrl").
-include("include/transfer_record.hrl").

%%%===================================================================
%%% API
%%%===================================================================

add_master(Server) ->
    io:format("Adding master server info.~n"),
    gen_server:call(?SERVER, {add_master, Server}).

get_servers() ->
    io:format("Updating record of servers.~n"),
    gen_server:call(?SERVER, {get_servers}).

update(File_Name, Data) ->
    io:format("Updating file: ~p.~n", [File_Name]),
    gen_server:cast(?SERVER, {update, File_Name, Data}).

lookup(File_Name) ->
    io:format("Looking up file info: ~p.~n", [File_Name]),
    gen_server:call(?SERVER, {lookup, File_Name}).

delete(File_Name) ->
    io:format("Deleting file: ~p.~n", [File_Name]),
    gen_server:cast(?SERVER, {delete, File_Name}).

get_files() ->
    io:format("Getting all files on record.~n"),
    gen_server:call(?SERVER, {get_files}).
    
get_updates(From) ->
    gen_server:call(?SERVER, {get_updates, From}).

insert_file(File, Value) ->
    gen_server:cast(?SERVER, {insert, File, Value}).

add_backup({Node_Name, IP, Port, Sctp_Port}) when is_list(Node_Name) ->
    add_backup({list_to_atom(Node_Name), IP, Port, Sctp_Port});
add_backup({Node_Name, IP, Port, Sctp_Port}) ->
    io:format("Adding backup server info.~n"),
    gen_server:call(?SERVER, 
		    {add_backup, {Node_Name, IP, Port, Sctp_Port}}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Servers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Servers], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([Servers]) ->
    process_flag(trap_exit, true),
    {Name, IP, _Port, Sctp_Port} = Servers#servers.backup,

    {ok, Port} = application:get_env(master, default_port),

    {ok, _Socket} = gen_udp:open(Port, [{active, true}, binary, 
					{reuseaddr, true}]),
    
    Servers1 = Servers#servers{backup=undefined,
			      socket=undefined,
			      ip=IP,
			      port=Sctp_Port,
			      master={Name}},

    {ok, Servers1};

init([]) ->
    process_flag(trap_exit, true),
    {ok, Port} = application:get_env(master, default_port),

    {ok, _Socket} = gen_udp:open(Port, [{active, true}, binary, 
					{reuseaddr, true}]),
    {ok, #servers{fileservers=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({add_master, #servers{socket=Socket, ip=IP, port=Port, 
				  master=Master}}, _From, Server) ->
    Server2 = Server#servers{socket=Socket, ip=IP, port=Port, 
			     master=Master},
    {reply, Server2, Server2};

handle_call({add_backup, Backup}, _From, Server) ->
    Server2 = Server#servers{backup = Backup},
    {reply, Server2, Server2};

handle_call({get_servers}, _From, Servers) ->
    {reply, Servers, Servers};

handle_call({lookup, File_Name}, _From, Servers) ->
    {reply, master_store:lookup(File_Name), Servers};

handle_call({get_files}, _From, Servers) ->
    {reply, master_store:get_match({'$1', '_', '_', '_'}), Servers};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% Add check for when a backup already exists.
handle_cast({backup, {Name, IP, Port, Sctp_Port}}, Servers) ->
    case ping_node(Name) of
	{error, timeout} ->
	    {noreply, Servers};
	ok ->
	    Servers1 = Servers#servers{backup={Name, IP, Port, Sctp_Port}},
	    backup_server:add_servers(Name, Servers1),
	    {noreply, Servers1}
    end;

handle_cast({update, File_Name, Data}, Server) ->
    master_store:update(File_Name, Data),
    {noreply, Server};

handle_cast({delete, File_Name}, Server) ->
    master_store:delete(File_Name),
    {noreply, Server};

handle_cast({insert, File_Name, Value}, Server) ->
    master_store:insert(File_Name, Value),
    {noreply, Server};

handle_cast(Msg, State) ->
    io:format("Invalid Request: ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({udp, Socket, IP, Port, Packet}, 
	    #servers{backup=Backup, fileservers = File_Servers}=Server) ->
    io:format("UDP packet received from ~p:~p~n", [IP, Port]),

    case Packet of
	<<0:8, Sctp_Port:16, Name_Bin/binary>> = Packet ->
	    Name = binary_to_list(Name_Bin),

	    case Backup of
		undefined ->
		    %% Backup
		    io:format("Designating backup role.~n"),
		    
		    Node_Name = list_to_atom(Name),
		    io:format("Node Name: ~p~n", [Node_Name]),


		    %% Send back name of Current Master Node. 1 to indicate backup
		    Master_Node = list_to_binary(atom_to_list(node())),
		    Packet1 = list_to_binary([<<1>>, Master_Node]),
		    gen_udp:send(Socket, IP, Port, Packet1),

		    Servers1 = Server#servers{
				 backup={Node_Name, IP, Port, Sctp_Port},
				 fileservers = [{Node_Name, IP, Port, Sctp_Port} | File_Servers]},
		    timer:sleep(100),
		    gen_balance:add_backup(Node_Name),

		    %% Add name to file server list too.
		    %% Save name as string like other servers.
		    gen_balance:add_server({Name, IP, Port, Sctp_Port}),

		    {noreply, Servers1};

		_Defined ->
		    %% File_Server
		    io:format("Designating file server role.~n"),
		    {_BNode, BIP, _BPort, BSctp_Port} = Server#servers.backup,
		    {BIP1, BIP2, BIP3, BIP4} = BIP,
		    Packet1 = list_to_binary([<<3>>, <<BIP1>>, <<BIP2>>, 
					      <<BIP3>>, <<BIP4>>, <<BSctp_Port>>]),
		    gen_udp:send(Socket, IP, Port, Packet1),
		    
		    gen_balance:add_server({Name, IP, Port, Sctp_Port}),

		    File_Servers = Server#servers.fileservers,
		    Servers1 = Server#servers{fileservers=
						  [{Name, IP, Port, Sctp_Port} | File_Servers]},

		    {Backup_Node, _, _, _} = Backup,
		    backup_server:add_servers(Backup_Node, Servers1),

		    {noreply, Servers1}
	    end;

	_Other ->
	    io:format("Backup info: ~p~n", [Backup]),
	    {Backup_Node, _, _, _} = Backup,

	    case transport:from_header(Packet) of
		{dlt, Filename} -> 
		    master_store:delete(binary_to_list(Filename)),

		    backup_server:delete_file(Backup_Node, Filename),

		    gen_udp:send(Socket, IP, Port, Packet),
		    {noreply, Server};

		{updtts, Filename, {IP, Sctp_Port}, {Sec_IP, Sec_Sctp_Port}} ->
		    case master_store:lookup(Filename) of
			{ok, {Timestamp, {_IP1, _Port1}, Secondary}} ->
			    io:format("Lookup for ~p: ~p~n", [Filename, {IP, Sctp_Port}]),
			    master_store:update(Filename, {Timestamp + 1, 
							   {IP, Sctp_Port}, Secondary}),
			    
			    backup_server:update_file(Backup_Node, 
						 {Filename, Timestamp + 1,
						  {IP, Sctp_Port}, Secondary}),
			    
			    gen_udp:send(Socket, IP, Port, Packet);
			_New ->
			    io:format("Lookup for ~p: ~p~n", [Filename, {IP, Sctp_Port}]),
			    master_store:update(Filename, {1, {IP, Sctp_Port}, {Sec_IP, Sec_Sctp_Port}}),
			    io:format("Notifying Backup node: ~p~n", [Backup_Node]),
			    
			    backup_server:new_file(Backup_Node, 
						   {Filename, 1, {IP, Sctp_Port}, 
						    {Sec_IP, Sec_Sctp_Port}}),
			    
			    gen_udp:send(Socket, IP, Port, Packet)
		    end,
		    {noreply, Server};

		{getfloc, Filename} ->
		    {ok, {_Timestamp, Loc, _Secondary}} = master_store:lookup(Filename),

		    Packet1 = transport:to_header({?SNDFLOC, Filename, Loc}),
		    gen_udp:send(Socket, IP, Port, Packet1),
		    {noreply, Server};

		{newsecfloc, _Filename, Filesize} ->
		    {secondary, New_Sec} = gen_balance:get_new_secondary({IP, Port}, Filesize),
		    Packet1 = transport:to_header({?SNDSEC, New_Sec}),
		    io:format("Sending back secondary: ~p~n", [Packet1]),
		    gen_udp:send(Socket, IP, Port, Packet1),
		    {noreply, Server};
		Other -> 
		    io:format("Error received type: ~p~n", [Other]),
		    {noreply, Server}
	    end
    end.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
    
%% role_server(Port) ->
%%     {ok, Socket} = gen_udp:open(Port),
%%     role_server(role, Socket).

%% role_server(role, Socket) ->
%%     case gen_udp:recv(Socket, 1) of
%% 	{ok, {Address, Port, Packet}} ->
%% 	    Role = get_role(Address, Port),
%% 	    gen_udp:send(Socket, Address, Port, Role),
%% 	    role_server(role, Socket);
%% 	{error, Reason} ->
%% 	    io:format("Error receiving role request: ~p~n", [Reason]),
%% 	    role_server(role, Socket)
%%     end.
    
ping_node(Name) ->
    ping_node(Name, 5).

ping_node(_Name, 0) ->
    {error, timeout};
ping_node(Name, Attempt) ->
    case net_adm:ping(Name) of
	pong ->
	    io:format("Connected to ~p~n", [Name]),
	    ok;
	pang ->
	    io:format("Pang~n"),
	    timer:sleep(100),
	    ping_node(Name, Attempt-1)
    end.

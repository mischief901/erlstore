%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created :  5 Apr 2018 by Alex Misch <alex@alex-Lenovo>
%%%------------------------------------------------------------------
-module(download).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([get_file/1, new_file/3, delete_file/1, get_all_files/0,
	 update_file/2, add_master/1]).

-export([download/1, upload/2, update/2]).

-define(SERVER, ?MODULE).

-include("transfer_record.hrl").
-include("server_record.hrl").
-include("inet.hrl").
-include("inet_sctp.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

add_master({Master_IP, Master_Port}) ->
    io:format("~p:~p~n", [Master_IP, Master_Port]),
    {ok, IP} = inet:parse_address(binary_to_list(Master_IP)),
    Port = list_to_integer(binary_to_list(Master_Port)),
    gen_server:cast(?SERVER, {add_master, IP, Port}).

get_file(File_Name) ->
    File_Name1 = binary_to_list(File_Name),
    Location = gen_server:call(?SERVER, {get_file_loc, File_Name1}),
    gen_server:call(?SERVER, {download, File_Name1, Location}, infinity).

new_file(File_Name, File_Location, Filesize) when is_binary(File_Name) ->
    new_file(binary_to_list(File_Name), binary_to_list(File_Location), Filesize);

new_file(File_Name, File_Location, Filesize) when is_list(File_Name) ->
    io:format("Getting new file location.~n"),
    Location = gen_server:call(?SERVER, {new_file_loc, File_Name, Filesize}, infinity),

    io:format("Location received: ~p. Uploading file.~n", [Location]),
    gen_server:call(?SERVER, {upload, File_Name, File_Location, Location}, infinity).


delete_file(File_Name) when is_binary(File_Name) ->
    delete_file(binary_to_list(File_Name));
delete_file(File_Name) ->
    Location = gen_server:call(?SERVER, {get_file_loc, File_Name}),
    gen_server:call(?SERVER, {delete, File_Name, Location}, infinity).

get_all_files() ->
    gen_server:call(?SERVER, {get_all_files}, infinity).

update_file(File_Name, File_Location) when is_binary(File_Name) ->
    update_file(binary_to_list(File_Name), File_Location);
update_file(File_Name, File_Location) ->
    Location = gen_server:call(?SERVER, {get_file_loc, File_Name}),
    gen_server:call(?SERVER, {update, File_Name, File_Location, Location}, infinity).
    

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

init([]) ->
    process_flag(trap_exit, true),

    {ok, Socket} = gen_sctp:open([{active, false},
				  {recbuf, 65536},
				  {reuseaddr, true},
				  {ip, {127,0,0,1}},
				  {sctp_events, 
				   #sctp_event_subscribe{shutdown_event=true}}]),

    {ok, {IP, Port}} = inet:sockname(Socket),
    io:format("Default transfer Socket opened at ~p:~p~n", [IP, Port]),

    Transfer_Default = #transfer{socket=Socket, server=IP, port=Port},
    {ok, {#transfer{}, Transfer_Default}}.

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

handle_call({get_file_loc, File_Name}, _From, 
	    {#transfer{socket=Socket, assoc=Assoc}, _Transfer}=State) ->

    Request = transport:to_header({?GETFLOC, File_Name}),
    gen_sctp:send(Socket, Assoc, 0, Request),

    Response = 
	case gen_sctp:recv(Socket) of
	    {ok, {_IP, _Port, _, Packet}} ->
		{sndfloc, _Filename, Location} = transport:from_header(Packet),
		Location;
	    Other ->
		io:format("Error getting file location: ~p~n", [Other]),
		{error, not_found}
	end,

    {reply, Response, State};


handle_call({new_file_loc, File_Name, Filesize}, _From, 
	    {#transfer{socket=Socket, assoc=Assoc}, _Transfer}=State) ->

    io:format("Getting header for ~p location request.~n", [File_Name]),

    Request = transport:to_header({?NEWFLOC, File_Name, Filesize}),

    io:format("Sending request to Master: ~p~n", [Request]),

    gen_sctp:send(Socket, Assoc, 1, Request),
    
    io:format("Getting request from master.~n"),
    
    {ok, {sndfloc, File_Name, Location}} = receive_response(Socket),
        
    {reply, Location, State};

handle_call({get_all_files}, _From, 
	    {#transfer{socket=Socket,
		      server=IP, port=Port, assoc=Assoc}, _Transfer}=State) ->
    
    Request = transport:to_header({?SNDLIST}),
    gen_sctp:send(Socket, Assoc, 0, Request),

    Response =
	case gen_sctp:recv(Socket) of
	    {ok, {IP, Port, Assoc, Packet}} ->
		{data, Data} = transport:from_header(Packet),
		Data;
	    Other ->
		io:format("Error getting file list: ~p~n", [Other]),
		{error, not_found}
	end,
    
    {reply, Response, State};

handle_call({upload, File_Name, File_Location, {IP, Port}}, _From,
	    {_Master, Transfer}=State) ->

    io:format("File_Location: ~p~n", [File_Location]),

    Transfer1 = Transfer#transfer{server=IP, port=Port, file=File_Name},
    upload(File_Location, Transfer1),
    {reply, ok, State};

handle_call({download, File_Name, {IP, Port}}, _From,
	    {_Master, Transfer}=State) ->

    Transfer1 = Transfer#transfer{server=IP, port=Port, file=File_Name},
    download(Transfer1),
    {reply, ok, State};

handle_call({delete, File_Name, {IP, Port}}, _From, 
 	    {_Master, #transfer{socket=Socket}}=State) ->
	    
    {ok, Assoc} = gen_sctp:connect(Socket, IP, Port, [{active, false}]),    
    Request = transport:to_header({?DLT, File_Name}),
    gen_sctp:send(Socket, Assoc, 0, Request),

    {reply, ok, State};

handle_call({update, File_Name, File_Location, {IP, Port}}, _From,
	    {_Master, Transfer}=State) ->

    io:format("File_Location: ~p~n", [File_Location]),

    Transfer1 = Transfer#transfer{server=IP, port=Port, file=File_Name},
    update(File_Location, Transfer1),
    {reply, ok, State};


handle_call(Request, _From, State) ->
    io:format("Weird Request: ~p~n", [Request]),
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

handle_cast({add_master, Master_IP, Master_Port}, {_, Transfer_Default}) ->
    {ok, Socket} = gen_sctp:open([{active, false},
				  {recbuf, 65536},
				  {reuseaddr, true}, {ip, Master_IP}]),

    {ok, Assoc} = gen_sctp:connect(Socket, Master_IP, Master_Port, 
				   [{active, false}]),
    Master = #transfer{socket=Socket, assoc=Assoc, 
		       server=Master_IP, port=Master_Port},
    
    io:format("Connected to Master: ~p~n", [Master]),

    {noreply, {Master, Transfer_Default}};

handle_cast(Msg, State) ->
    io:format("Weird Message: ~p~n", [Msg]),
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
handle_info(_Info, State) ->
    {noreply, State}.

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

download(#transfer{socket=Socket, file=File_Name}=Transfer) ->

    File_Path = transport:get_file_path(File_Name),
    {ok, FD} = file:open(File_Path, [binary, raw, write]),
    Transfer1 = Transfer#transfer{fd=FD},

    gen_sctp:recv(Socket, 500),
    Transfer2 = connect_socket(Transfer1),

    Data = transport:read_file(Transfer2),
    transport:write_data(FD, Data),

    io:format("~p Downloaded to proxy.~n", [File_Name]),
    file:close(FD),

    gen_sctp:recv(Socket),
    gen_sctp:recv(Socket),

    {ok, File_Name}.

upload(File_Location, #transfer{socket=Socket} =Transfer) ->

    {ok, FD} = file:open(File_Location, [binary, raw, read]),

    File_size=filelib:file_size(File_Location),

    Transfer1 = connect_socket(Transfer#transfer{fd=FD, filesize=File_size}),

    transport:send_file(Transfer1),
    file:close(FD),

    gen_sctp:recv(Socket),
    gen_sctp:recv(Socket),

    io:format("File uploaded. Socket reset.~n").

update(File_Location, Transfer) ->	    

    upload(File_Location, Transfer),

    io:format("File updated~n").

connect_socket(#transfer{server=IP, port=Port, socket=Socket,
			 timestamp=Timestamp, assoc=Assoc1} = Transfer) ->	    
    
    io:format("~p~n", [gen_sctp:recv(Socket, 10)]),

    case gen_sctp:connect(Socket, IP, Port, [{active, false},
					     {reuseaddr, true}]) of
	{ok, Assoc} ->
	    Transfer#transfer{assoc=Assoc, timestamp=Timestamp+1};
	
	{error, eisconn} ->
	    io:format("Already Connected~n"),
	    {ok, [{sctp_associnfo, Assoc_info}]} = inet:getopts(Socket, [sctp_associnfo]),
	    io:format("opts: ~p~n", [Assoc_info]),
	    Transfer#transfer{assoc=Assoc_info#sctp_assocparams.assoc_id};
	
	{error, {_,_,_,{sctp_shutdown_event,_Assoc}}} ->
	    io:format("Shutdown Event. Attempting to recover. ~n"),
	    connect_socket(Transfer);

	{error,{_,_,_,{sctp_paddr_change,_,_,_,Assoc_id}}} ->
	    io:format("paddr change.~n"),
	    Transfer#transfer{assoc=Assoc1#sctp_assoc_change{assoc_id=Assoc_id}};

	Other ->
	    io:format("Other when connecting: ~p~n", [Other]),
	    io:format("Trying again.~n"),
	    connect_socket(Transfer)
    end.

receive_response(Socket) ->    
    	case gen_sctp:recv(Socket) of

	    {ok, { _IP, _Port, _, {sctp_assoc_change, shutdown_comp, _, _, _, _}}} ->
		io:format("Shutdown_Comp~n"),
		receive_response(Socket);

	    {ok, {_IP, Port, _, {sctp_paddr_change, {IP2, Port}, addr_confirmed, _, _}}} ->
	    	io:format("Changing to new IP: ~p~n", [IP2]),
	    	receive_response(Socket);

	    {ok, {_IP, _Port, Assoc, Packet}} ->
		io:format("Assoc: ~p~nPacket: ~p~n", [Assoc, Packet]),
	        {ok, transport:from_header(Packet)};

	    Other ->
		io:format("Error new file location: ~p~n", [Other]),
		{error, not_found}

	end.

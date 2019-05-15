%%%-------------------------------------------------------------------
%%% @author alex <alex@alex-Lenovo>
%%% @copyright (C) 2018, alex
%%% @doc
%%%
%%% @end
%%% Created : 11 Apr 2018 by alex <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(file_transfer).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-export([next_state/1]).

-define(SERVER, ?MODULE).

-include("include/inet.hrl").
-include("include/inet_sctp.hrl").
-include("include/transfer_record.hrl").

%%%===================================================================
%%% API
%%%===================================================================

next_state(Pid) ->
    gen_server:call(Pid, next_state, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

start_link({Event, Transfer, Master}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
			  [{Event, Transfer, Master}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------

init([{Event, Transfer, {Master_IP, Master_Port}}]) ->
    process_flag(trap_exit, true),
    
    {ok, Port} = application:get_env(file, port),

    {ok, Socket} = gen_udp:open(Port, [{active, false},  binary]),
    
    {ok, {init, {Event, Transfer, {Socket, Master_IP, Master_Port}}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------

handle_call(next_state, _From, {init, {delete, Transfer, Master}}) ->
    File = Transfer#transfer.file,
    io:format("Deleting file: ~p~n", [File]),
    File_Path = transport:get_file_path(File),
    case file:delete(File_Path) of
	ok ->
	    io:format("File deleted successfully.~n"),    
	    {reply, ok, {update_master, {delete, Transfer, Master}}};
	{error, Reason} ->
	    io:format("File not deleted: ~p~n", [Reason]),
	    {reply, error, {error, Transfer, Reason, Master}}
    end;

handle_call(next_state, _From, {init, {Event, Transfer, Master}}) ->
    File = Transfer#transfer.file,
    io:format("Performing ~p on ~p~n", [Event, File]),
    {reply, ok, {open, {Event, Transfer, Master}}};

handle_call(next_state, _From, {open, {read, Transfer, Master}}) ->
    File = Transfer#transfer.file,
    File_Path = transport:get_file_path(File),

    Filesize = filelib:file_size(File_Path),

    case file:open(File_Path, [raw, binary, read]) of
	{ok, FD} ->
	    Transfer1 = Transfer#transfer{fd=FD, filesize = Filesize},
	    io:format("Opened, Reading.~n"),
	    {reply, ok, {read, Transfer1, Master}};
	{error, Reason} ->
	    io:format("Error opening file: ~p~n", [Reason]),
	    {reply, error, {error, Transfer, Reason, Master}}
    end;

handle_call(next_state, _From, {open, {write, Transfer, Master}}) ->
    File = Transfer#transfer.file,

    File_Path = transport:get_file_path(File),
    io:format("Opening ~p for writing.~n", [File_Path]),
    
    case file:open(File_Path, [raw, binary, write, read, delayed_write]) of
	{ok, FD} ->
	    Transfer1 = Transfer#transfer{fd=FD},
	    io:format("Opened, Writing.~n"),
	    {reply, ok, {write, Transfer1, Master}};
	{error, Reason} ->
	    io:format("Error opening file: ~p~n", [Reason]),
	    {reply, error, {error, Transfer, Reason, Master}}
    end;

handle_call(next_state, _From, 
	    {open, {write_secondary,  Transfer, Master}}) ->
    File = Transfer#transfer.file,

    File_Path = transport:get_file_path(File),
    io:format("Opening ~p for writing.~n", [File_Path]),
    
    case file:open(File_Path, [raw, binary, write, read, delayed_write]) of
	{ok, FD} ->
	    Transfer1 = Transfer#transfer{fd=FD},
	    io:format("Opened, Writing.~n"),
	    {reply, ok, {write_secondary, Transfer1, Master}};
	{error, Reason} ->
	    io:format("Error opening file: ~p~n", [Reason]),
	    {reply, error, {error, Transfer, Reason, Master}}
    end;    

handle_call(next_state, _From, {write, Transfer, Master}) ->
    io:format("Starting to write file~n"),
    
    Data = transport:read_file(Transfer),

    transport:write_data(Transfer#transfer.fd, Data),

    File_Path = transport:get_file_path(Transfer#transfer.file),
    Transfer1 = Transfer#transfer{filesize=filelib:file_size(File_Path)},
    io:format("File transfer finished. Primary file written.~n"),
    {reply, ok, {update_secondary, Transfer1, Master}};


handle_call(next_state, _From, {write_secondary, Transfer, Master}) ->
    io:format("Starting to writing file~n"),
    
    Data = transport:receive_data(Transfer),
    transport:write_data(Transfer#transfer.fd, Data),
    File_Path = transport:get_file_path(Transfer#transfer.file),
    Transfer1 = Transfer#transfer{filesize=filelib:file_size(File_Path)},
    io:format("File transfer finished. Secondary file written.~n"),

    gen_sctp:eof(Transfer#transfer.socket, Transfer#transfer.assoc),
    {reply, ok, {close, Transfer1, Master}};

handle_call(next_state, _From, {read, Transfer, Master}) ->
    io:format("Starting to read file~n"),
    
    transport:send_file(Transfer),
    io:format("File transfer finished. Closing file.~n"),
    {reply, ok, {close, Transfer, Master}};

handle_call(next_state, _From, 
	    {update_master, {delete, Transfer, Master}}) ->
    io:format("Deleting from Master~n"),
    send_master(delete, Transfer#transfer.file, Master, 
		{Transfer#transfer.server, Transfer#transfer.port}),

    {stop, normal, done, {Transfer, Master}};

handle_call(next_state, _From, 
	    {update_secondary, Transfer, {_Sock, IP, Port} =Master}) ->
    io:format("Getting secondary locations from Master~n"),
    {ok, Secondaries} = get_secondary(Transfer#transfer.file, 
				      Transfer#transfer.filesize, Master),
    io:format("Secondaries: ~p~n", [Secondaries]),
    
    transfer_secondary(Transfer, Secondaries),
    file_store:insert(Transfer#transfer.file, 
		      {Transfer#transfer.timestamp, {IP, Port}, Secondaries}),

    io:format("Secondaries updates. Updating Master~n"),
    {reply, ok, {update_master, Transfer, Master}};

handle_call(next_state, _From, 
	    {update_master, Transfer, {Sock, IP, _Port}=Master}) ->

    io:format("Updating Master~n"),
    {ok, {_, SCTP_Port}} = inet:sockname(Transfer#transfer.socket),
    send_master(update, Transfer#transfer.file, {Sock, IP, SCTP_Port}, 
		{Transfer#transfer.server, Transfer#transfer.port}),
    {reply, ok, {close, Transfer, Master}};

handle_call(next_state, _From, 
	    {close, #transfer{fd=FD}=Transfer, {Socket, _IP, _Port}=Master}) ->

    io:format("Closing File~n"),
    file:close(FD),

    gen_udp:close(Socket),
    gen_sctp:eof(Transfer#transfer.socket, Transfer#transfer.assoc),
    {stop, normal, done, {Transfer, Master}};

handle_call(Request, _From, State) ->
    io:format("Error, Invalid request: ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
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
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_secondary(File_Name, Filesize, 
	      {Socket, _Master_IP, _Master_Port}) ->

    case file_store:lookup(File_Name) of
	{ok, {_Timestamp, _Primary, Secondary}} ->
	    {ok, Secondary};
	{error, not_found} ->

	    Request = transport:to_header({?NEWSECFLOC, File_Name, Filesize}),

	    {ok, {IP, Port}} = application:get_env(connect, master_info),
	    {ok, IP2} = inet:parse_address(IP),
	    {Port2, _Rest} = string:to_integer(Port),

	    gen_udp:send(Socket, IP2, Port2, Request),
	    receive_udp(Request, {Socket, IP2, Port2}, 5)
    end.

send_master(update, File_Name, {Socket, IP, SCTP_Port}=Master, Sec_Loc) ->
    Update = transport:to_header({?UPDTTS, File_Name, 
				  {IP, SCTP_Port}, Sec_Loc}),

    {ok, {Master_IP, Master_Port}} = application:get_env(connect, master_info),
    {ok, Master_IP2} = inet:parse_address(Master_IP),
    {Master_Port2, _Rest} = string:to_integer(Master_Port),

    gen_udp:send(Socket, Master_IP2, Master_Port2, Update),
    receive_udp(Update, Master, 5);

send_master(delete, File_Name, 
	    {Socket, Master_IP, Master_Port}=Master, _) ->

    Delete = transport:to_header({?DLT, File_Name}),
    gen_udp:send(Socket, Master_IP, Master_Port, Delete),
    receive_udp(Delete, Master, 5).

receive_udp(Request, {Socket, IP, SCTP_Port}, 0) ->
    
    case application:get_env(file, backup) of
	{ok, {IP, SCTP_Port}} ->
	    {error, timeout};

	{ok, {New_Master_IP, New_Master_Port}} ->
	    application:set_env(connect, master_info, 
				{New_Master_IP, New_Master_Port}),
	    {ok, New_Master_IP2} = inet:parse_address(New_Master_IP),
	    {New_Master_Port2, _Rest} = string:to_integer(New_Master_Port),
	    New_Master = {New_Master_IP2, 4010, New_Master_Port2},

	    gen_udp:send(Socket, New_Master_IP2, New_Master_Port2, Request),
	    receive_udp(Request, New_Master, 5)
    end;

receive_udp(Request, {Socket, Master_IP, Master_Port}=Master, Attempt) ->
    case gen_udp:recv(Socket, 0, 1000) of
	{error, timeout} ->
	    gen_udp:send(Socket, Master_IP, Master_Port, Request),
	    receive_udp(Request, Master, Attempt-1);

	{ok, {_IP, _Port, Packet}} ->
	    case transport:from_header(Packet) of
		{sndsec, _File_Name, Loc} ->
		    {ok, Loc};
		{updtts, File_Name, _, _} ->
		    io:format("File timestamp updated: ~p~n", [File_Name]),
		    ok;
		{dlt, File_Name} ->
		    io:format("File deleted: ~p~n", [File_Name]),
		    ok;
		{Request}->
		    ok
	    end
    end.	    

%% for a list of secondaries.
%transfer_secondaries(Transfer, [Secondary | Rest]) ->
    %transfer_secondary(Transfer, Secondary),
    %transfer_secondaries(Transfer, Rest);

%transfer_secondaries(_Transfer, []) ->
    %ok.

transfer_secondary(Transfer, {Server, Port}=Secondary) ->    
    case gen_sctp:connect(Transfer#transfer.socket, Server, 
			  Port, [{active, false}]) of
	{ok, Assoc} ->
	    Transfer1 = Transfer#transfer{server = Server, 
					  port = Port, assoc = Assoc},
	    transport:send_sec(Transfer1);
	{error, _} ->
	    transfer_secondary(Transfer, Secondary)
    end.
    

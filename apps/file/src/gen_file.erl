%%%-------------------------------------------------------------------
%%% @author Matthew Turner <mturne07@vm-hw09.eecs.tufts.edu>
%%% @copyright (C) 2018, Matthew Turner
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2018 by Matthew Turner <mturne07@vm-hw09.eecs.tufts.edu>
%%%-------------------------------------------------------------------
-module(gen_file).

%% -behaviour(gen_server).

-include("include/transfer_record.hrl").
-include_lib("include/inet.hrl").
-include_lib("include/inet_sctp.hrl").


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, format_status/2]).

-export([run_state/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

start_link({IP, Port}) ->
    Pid = spawn_link(?MODULE, init, [{IP, Port}]),
    {ok, Pid}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init(Master) ->
    {ok, Port} = application:get_env(file, sctp),
    {ok, Socket} = gen_sctp:open(Port, [{reuseaddr, true}, {active, false}, {recbuf, 65000}]),
    io:format("Socket opened on port: ~p~n", [Port]),
    Transfer = #transfer{socket = Socket},
    ok = gen_sctp:listen(Transfer#transfer.socket, true),

    server_loop(Transfer, Master).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
%% -spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
%% 			 {reply, Reply :: term(), NewState :: term()} |
%% 			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
%% 			 {reply, Reply :: term(), NewState :: term(), hibernate} |
%% 			 {noreply, NewState :: term()} |
%% 			 {noreply, NewState :: term(), Timeout :: timeout()} |
%% 			 {noreply, NewState :: term(), hibernate} |
%% 			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
%% 			 {stop, Reason :: term(), NewState :: term()}.
%% handle_call(_Request, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
%% -spec handle_cast(Request :: term(), State :: term()) ->
%% 			 {noreply, NewState :: term()} |
%% 			 {noreply, NewState :: term(), Timeout :: timeout()} |
%% 			 {noreply, NewState :: term(), hibernate} |
%% 			 {stop, Reason :: term(), NewState :: term()}.
%% handle_cast(_Request, State) ->
%%     {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
%% -spec handle_info(Info :: timeout() | term(), State :: term()) ->
%% 			 {noreply, NewState :: term()} |
%% 			 {noreply, NewState :: term(), Timeout :: timeout()} |
%% 			 {noreply, NewState :: term(), hibernate} |
%% 			 {stop, Reason :: normal | term(), NewState :: term()}.
%% handle_info(_Info, State) ->
%%     {noreply, State}.

server_loop(#transfer{socket = Socket, assoc = Asso} = Transfer, Master) ->
    
    case gen_sctp:recv(Socket) of
	{ok, {_, _, _, {sctp_assoc_change, shutdown_comp, _, _, _, _}}} ->
	    %io:format("Socket Assoc closed. Listening for new one~n"),
	    ok = gen_sctp:listen(Socket, true),
	    server_loop(Transfer, Master);

	{ok, {_,_,_, {sctp_assoc_change, comm_lost, _,_,_,_}}} ->
	    %io:format("Socket Assoc closed. Listening for new one*~n"),
	    ok = gen_sctp:listen(Socket, true),
	    server_loop(Transfer, Master);

	{ok, {_,_,_, {sctp_shutdown_event, _}}} ->
	    %io:format("Socket Assoc closed. Listening for new one.~n"),
	    ok = gen_sctp:listen(Socket, true),
	    server_loop(Transfer, Master);

	{ok, {FromIP, FromPort, _AncData, 
	      {sctp_assoc_change, comm_up, _, _, _, _}=A}} ->
	    io:format("Assoc: ~p~n", [A]),
	    io:format("Connection made with ~p:~p~n", [FromIP, FromPort]),
	    server_loop(Transfer#transfer{assoc=A}, Master);

	{ok, {_,_,_,{sctp_paddr_change,_,_,_,Four}}} ->
	    server_loop(Transfer#transfer{assoc=
					      Asso#sctp_assoc_change{assoc_id=Four}}, 
			Master);

	{ok, {FromIP, FromPort, _AncData, Data}} ->
	    Assoc = Transfer#transfer.assoc,

	    case transport:from_header(Data) of
		{dlt, Filename} ->
		    io:format("Deleting file: ~p~n", [Filename]),
		    Transfer1 = Transfer#transfer{server = FromIP,
						   port = FromPort,
						   assoc = Assoc,
						   file = Filename},
		    {ok, Pid} = supervisor:start_child(file_statem_sup, 
						       [{delete, Transfer1, Master}]),

		    run_state(Pid),
		    
		    server_loop(Transfer#transfer{assoc=undefined}, Master);

		{rrq, Window, Timestamp, Filename} ->
		    io:format("Read Request Received: ~p~n", [Filename]),
		    
		    Transfer1 = Transfer#transfer{server = FromIP,
						   port = FromPort,
						   assoc = Assoc,
						   file = Filename,
						   timestamp = Timestamp,
						   window = Window},
		    {ok, Pid} = supervisor:start_child(file_statem_sup, 
						       [{read, Transfer1, Master}]),
		    run_state(Pid),

		    timer:sleep(100),
		    gen_sctp:eof(Socket, Assoc),

		    supervisor:terminate_child(file_statem_sup, Pid),


		    server_loop(Transfer#transfer{assoc=undefined}, Master);

		{snd, Window, Timestamp, File_Name} ->
		    io:format("Send Request Received: ~p~n", [File_Name]),

		    Transfer1 = Transfer#transfer{server = FromIP,
						  port = FromPort,
						  assoc = Assoc,
						  timestamp = Timestamp,
						  window = Window,
						  file = File_Name},

		    io:format("Starting File Transfer~n"),

                    {ok, Pid} = supervisor:start_child(file_statem_sup, 
						       [{write, Transfer1, Master}]),

		    run_state(Pid),
		    
		    timer:sleep(100),
		    gen_sctp:eof(Socket, Assoc),

		    supervisor:terminate_child(file_statem_sup, Pid),
                    server_loop(Transfer#transfer{assoc=undefined}, Master);

		{sec, Window, Timestamp, File_Name} ->
		    io:format("Receiving secondary file: ~p~n", [File_Name]),

		    Transfer1 = Transfer#transfer{server = FromIP,
						  port = FromPort,
						  assoc = Assoc,
						  timestamp = Timestamp,
						  window = Window,
						  file = File_Name},

		    io:format("Starting Secondary File Transfer~n"),

                    {ok, Pid} = supervisor:start_child(file_statem_sup, 
						       [{write_secondary, Transfer1, Master}]),

		    run_state(Pid),

		    timer:sleep(100),
		    gen_sctp:eof(Socket, Assoc),

		    supervisor:terminate_child(file_statem_sup, Pid),
                    server_loop(Transfer#transfer{assoc=undefined}, Master);
			
		{error, Rest} ->
		    io:format("Error receiving in file_server: ~p~n", [Rest]),
		    timer:sleep(100),
		    server_loop(Transfer, Master);

		_Other ->
		    io:format("Throwing message out~n"),
		    server_loop(Transfer, Master)
	    end;

	{error, Reason} ->
	    io:format("Socket error in file_server: ~p~n", [Reason]),
	    timer:sleep(100),
	    server_loop(Transfer, Master)
    end.

		 
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

run_state(Pid) ->
    run_state(file_transfer:next_state(Pid), Pid).

run_state(ok, Pid) ->
    run_state(file_transfer:next_state(Pid), Pid);
run_state(done, _Pid) ->
    ok.


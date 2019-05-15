%%%-------------------------------------------------------------------
%%% @author alex <alex@alex-Lenovo>
%%% @copyright (C) 2018, alex
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2018 by alex <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(backup_server).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-export([add_servers/2, get_servers/1, new_file/2, update_file/2,
	 delete_file/2, update_balance_state/2]).

-define(SERVER, ?MODULE).

-include("server_record.hrl").

-record(state, {primaries=[], secondaries=[]}).

%%%===================================================================
%%% API
%%%===================================================================

add_servers(Node, Servers) ->
    io:format("Updating server list at backup: ~p~n", [Servers]),
    gen_server:cast({?SERVER, Node}, {add_servers, Servers}).

get_servers(Node) ->
    io:format("Getting server list from backup~n"),
    gen_server:call({?SERVER, Node}, get_servers).

new_file(Node, {File_Name, _, _, _} = File_Info) ->
    io:format("Sending new file info to backup: ~p~n", [File_Name]),
    gen_server:call({?SERVER, Node}, {new, File_Info}).

update_file(Node, {File_Name, _, _, _} = File_Info) ->
    io:format("Updating file info on backup: ~p~n", [File_Name]),
    gen_server:call({?SERVER, Node}, {update, File_Info}).

delete_file(Node, File_Name) ->
    io:format("Deleting file info on backup: ~p~n", [File_Name]),
    gen_server:call({?SERVER, Node}, {delete, File_Name}).

update_balance_state(Node, Balance_State) ->
    io:format("Updating backup info for balancing.~n"),
    gen_server:call({?SERVER, Node}, {update_balance, Balance_State}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

start_link({Servers, Balance}) ->
    gen_servers:start_link({local, ?SERVER}, ?MODULE, [Servers, Balance], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------

init([Servers, Balance]) ->
    process_flag(trap_exit, true),
    io:format("Starting new backup server~n"),
    {ok, {Servers, Balance}};

init([]) ->
    process_flag(trap_exit, true),
    io:format("Starting backup_server~n"),
    {ok, {#servers{}, #state{}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------

handle_call(get_servers, _From, {Servers, _Bal}=State) ->
    io:format("Sending servers~n"),
    {reply, Servers, State};

handle_call({new, {File_Name, Timestamp, Primary,  Secondary}}, 
	    _From, State) ->
    io:format("New file: ~p~n", [File_Name]),
    master_store:insert(File_Name, {Timestamp, Primary, Secondary}),
    {reply, ok, State};

handle_call({update, {File_Name, Timestamp, Primary, Secondary}},
	    _From, State) ->
    io:format("Updating file: ~p~n", [File_Name]),
    master_store:update(File_Name, {Timestamp, Primary, Secondary}),
    {reply, ok, State};

handle_call({delete, File_Name}, _From, State) ->
    io:format("Deleting file: ~p~n", [File_Name]),
    master_store:delete(File_Name),
    {reply, ok, State};

handle_call({update_balance, Balance_State}, _From, {Servers, _Old_Balance}) ->
    io:format("Balance state updated~n"),
    {reply, ok, {Servers, Balance_State}};

handle_call(Request, _From, State) ->
    io:format("Weird Request: ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------

handle_cast({add_servers, Servers}, {_Old_Servers, Balance}) ->
    io:format("Adding servers: ~p~n", [Servers]),
    {Master_Node} = Servers#servers.master,

    io:format("Monitoring ~p: ~p~n", [Master_Node,
				      monitor_node(Master_Node, true)]),
    
    {noreply, {Servers, Balance}};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------

handle_info({nodedown, _Master}, State) ->
    io:format("Master Node failed. Restarting Master here.~n"),
    
    supervisor:start_child(master_delay_start, [takeover, State]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Irregular request: ~p~n", [Info]),
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

%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(gen_balance).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([rebalance/0, get_new_primary/1, get_new_secondary/2, 
	 add_server/1, remove_server/1, add_backup/1, new_backup/0,
	 update_backup/1, update_backup/2]).

-define(SERVER, ?MODULE).

-record(state, {primaries=[], secondaries = [], backup}).

%%%===================================================================
%%% API
%%%===================================================================

rebalance() ->
    io:format("Rebalancing.~n"),
    gen_server:cast(?SERVER, {rebalance}).
    
add_server({Name, IP, Udp_Port, Port}= Server) ->
    io:format("Adding Server: ~p~n", [Server]),
    gen_server:cast(?SERVER, {add, {Name, IP, Udp_Port, Port, 0}}).

remove_server(Server) ->
    io:format("Removing Server: ~p~n", [Server]),
    gen_server:cast(?SERVER, {remove, Server}).

get_new_primary(File_Size) ->
    io:format("Getting next primary server.~n"),
    gen_server:call(?SERVER, {next_primary, File_Size}).

get_new_secondary(Primary, File_Size) ->
    io:format("Getting next secondary server.~n"),
    gen_server:call(?SERVER, {next_secondary, Primary, File_Size}).

add_backup(Backup) ->
    io:format("Updating backup Location~n"),
    gen_server:call(?SERVER, {add_backup, Backup}).

new_backup() ->
    io:format("Getting new backup location~n"),
    gen_server:call(?SERVER, {new_backup}).

update_backup(Node) ->
    gen_server:call(?SERVER, {update_backup, Node}).

update_backup(Node, State) ->
    backup_server:update_balance_state(Node, State).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(Servers) ->
    io:format("Starting balance gen_server on switch-over~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Servers], []).

start_link() ->
    io:format("Starting balance gen_server with default~n"),
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
init([]) ->
    {ok, #state{}};
init([Balance_Info]) ->
    process_flag(trap_exit, true),
    {ok, Balance_Info}.


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

handle_call({next_primary, File_Size}, _From, 
	    #state{primaries=Primaries} = State) ->

    [{Name, IP, _udp_Port, Port, Prim_Size}|Rest] = lists:keysort(4, Primaries),
    Prim_Size1 = Prim_Size + File_Size,

    Next = {Name, IP, _udp_Port, Port, Prim_Size1},
    State1 = State#state{primaries = [Next|Rest]},

    io:format("Next Primary: ~p~n", [Next]),
    update_backup(State#state.backup, State1),

    {reply, {primary, {IP, Port}}, State1};

%% When the next primary is not the next secondary.
%% Take the top of both primary and secondary.

handle_call({next_secondary, Primary, File_Size}, _From, 
	    #state{secondaries = Secondaries} = State) ->

    io:format("All secondaries: ~p~n", [State#state.secondaries]),
    {PIP, PPort} = Primary,
    case lists:keysort(4, Secondaries) of
	
	[{_, PIP, PPort, _, _}=Prim, {Name, IP, _udp_Port, Port, Sec_Size}|Rest] ->
	    Sec_Size1 = Sec_Size + File_Size,
	    Next = {Name, IP, _udp_Port, Port, Sec_Size1},
	    io:format("sending next secondary: ~p~n", [Next]),
	    State1 = State#state{secondaries = [Prim, Next | Rest]},

	    update_backup(State#state.backup, State1),

	    {reply, {secondary, {IP, Port}}, State1};
	
	[{_, PIP, PPort, _, _}] ->
	    {reply, {error, Primary}, State};
	[{Name, IP, Udp_Port, Port, Sec_Size}|Rest] ->
	    Sec_Size1 = Sec_Size + File_Size,
	    Next = {Name, IP, Udp_Port, Port, Sec_Size1},
	    io:format("sending next secondary: ~p~n", [Next]),
	    State1 = State#state{secondaries = [Next | Rest]},

	    update_backup(State#state.backup, State1),

	    {reply, {secondary, {IP, Port}}, State1}
	
    end;

handle_call({add_backup, Backup}, _From, State) ->
    New_State = State#state{backup=Backup},
    {reply, ok, New_State};

handle_call({new_backup}, _From, #state{primaries=Primaries}=State) ->
    
    New_Backup = hd(Primaries),
    {Name, _, _, _, _} = New_Backup,

    case node() of
	Name ->
	    io:format("New backup designated: ~p~n", [New_Backup]),
	    {reply, hd(tl(Primaries)), State};
	_Other ->
	    io:format("New backup designated: ~p~n", [New_Backup]),
	    {reply, New_Backup, State}
    end;

handle_call({update_backup, Node}, _From, State) ->
    update_backup(Node, State),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @Private
%% @Doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% Add a server to the primary and secondary pools
handle_cast({add, Server}, #state{primaries=Primaries,
				  secondaries=Secondaries}=State) ->
    State1 = State#state{primaries=[Server | Primaries],
			 secondaries= Secondaries++[Server]},

    io:format("Current server list: ~p~n", [State1#state.primaries]),

    update_backup(State#state.backup, State1),

    {noreply, State1};

%% Remove a server from the primary and secondary pools
handle_cast({remove, Server}, #state{primaries=Primaries,
				     secondaries=Secondaries}=State) ->
    Primaries1 = lists:delete(Server, Primaries),
    Secondaries1 = lists:delete(Server, Secondaries),
    State1 = State#state{primaries=Primaries1,
			     secondaries=Secondaries1},
    {noreply, State1}.

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


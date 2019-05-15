%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created : 31 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(master_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(takeover, State) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [State]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------

init([{Servers, Balance_Info}]) ->
    io:format("Initializing new master node with: ~p ~p~n", 
	      [Servers, Balance_Info]),

    io:format("Interface restart: ~p~n", [application:start(interface)]),
    
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    %% Master process
    Child3 = #{id => master,
	       start => {master_server, start_link, [Servers]},
	       restart => transient,
	       shutdown => 5000,
	       type => worker,
	       modules => [master_server]},

    %% Balancer supervisor tree
    Child2 = #{id => balancer,
	       start => {balancer_sup, start_link, [Balance_Info]},
	       restart => permanent,
	       shutdown => 5000,
	       type => supervisor,
	       modules => [balancer_sup, gen_balance]},

    Child1 = #{id => gen_update, 
	       start => {gen_update, start_link, [Servers]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [gen_update]},

    io:format("Supervisor initializing~n"),

    {ok, {SupFlags, [Child1, Child2, Child3]}};

init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    %% Master process
    Child3 = #{id => master,
	       start => {master_server, start_link, []},
	       restart => transient,
	       shutdown => 5000,
	       type => worker,
	       modules => [master_server]},

    %% Balancer supervisor tree
    Child2 = #{id => balancer,
	       start => {balancer_sup, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => supervisor,
	       modules => [balancer_sup, gen_balance]},

    Child1 = #{id => gen_update, 
	       start => {gen_update, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [gen_update]},

    io:format("Supervisor initializing~n"),

    {ok, {SupFlags, [Child1, Child2, Child3]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%

%%% Tufts COMP 112 - Networks
%%% Spring 2018
%%% Project 1
%%% Erlstore

%%% Name: Alex Misch
%%% Email: Alexander.Misch@tufts.edu
%%% UTLN: amisch01

%%% Name: Matt Turner
%%% Email: Matthew.Turner@tufts.edu
%%% UTLN: mturne07

%%% balancer_sup.erl

%%% Erlang supervisor which spawns a balancer process to help deal with
%%% file load across the system. On failure of the balancer, the supervisor
%%% recovers and spawns another.

%%%

-module(balancer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

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

start_link(Servers) ->
    io:format("Balancer supervisor starting on switch-over~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Servers]).

start_link() ->
    io:format("Balancer supervisor starting.~n"),
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

init([Balance_Info]) ->
    
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => balancer,
	       start => {gen_balance, start_link, [Balance_Info]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [gen_balance]},

    {ok, {SupFlags, [AChild]}};

init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => balancer,
	       start => {gen_balance, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [gen_balance]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(backup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

init([Servers, Balance]) ->

    io:format("Starting new backup supervisor~n"),
    
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Child = #{id => backup_server,
	      start => {backup_server, start_link, [{Servers, Balance}]},
	      restart => permanent,
	      shutdown => 5000,
	      type => worker, 
	      modules => [backup_server]},

    {ok, {SupFlags, [Child]}};

init([]) ->
    
    io:format("Trying to connect to Master Server.~n"),
    io:format("~p~n", [{ok, Master} = application:get_env(backup, master_node)]),
    io:format("~p~n", [net_kernel:connect_node(Master)]),
    %% Directly connected to Master server.
    io:format("Connected to Master Server.~n"),

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Child = #{id => backup_server,
	      start => {backup_server, start_link, []},
	      restart => permanent,
	      shutdown => 5000,
	      type => worker, 
	      modules => [backup_server]},
    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


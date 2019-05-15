%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(file_sup).

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
    io:format("Starting File Supervisor.~n"),
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
init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Child1 = #{id => file_statem_sup,
	       start => {file_statem_sup, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => supervisor,
	       modules => [file_statem_sup, file_transfer]},

    %% Need to get the IP and Port from the environment variables
    {ok, {IP, Port}} = get_master(),

    Child2 = #{id => gen_file,
	       start => {gen_file, start_link, [{IP, Port}]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [gen_file]},

    {ok, {SupFlags, [Child1, Child2]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_master() ->
    case application:get_env(file, server) of
	{ok, {IP, Port}} ->
	    {ok, {IP, Port}};
	{ok, [[{IP_raw, Port}]]} ->
	    {ok, IP} = inet:parse_address(IP_raw),
	    {ok, {IP, list_to_integer(Port)}};
	Other ->
	    io:format("Master server information not found: ~p~n", [Other]),
	    {error, not_found}
    end.

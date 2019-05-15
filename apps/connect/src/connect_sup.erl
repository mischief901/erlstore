%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(connect_sup).

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
init([]) ->
    %% Make sure all application processes are loaded by the VM.
    %% io:format("Loaded Applications: ~p~n", [application:loaded_applications()]),
    io:format("~p~n", [application:ensure_all_started(backup)]),
    io:format("~p~n", [application:ensure_all_started(master)]),
    io:format("~p~n", [application:ensure_all_started(file)]),

    %% Get Command-Line arguments for where to connect.
    %% Change to allow for a master flag that invalidates asking
    %% for the server_ip and server_port (since it's not necessary).
    case get_arg(master) of
	{ok, false} ->
	    %% {ok, Server_IP} = get_arg(server_ip),
	    %% {ok, Server_Port} = get_arg(server_port),
	    {ok, {Server_IP, Server_Port}} = get_arg(master_info),
	    {ok, Port} = get_arg(default_port),
	    io:format("get arg results:~n Server_IP: ~p~n Server_Port: ~p~n Port: ~p~n", [Server_IP, Server_Port, Port]),
	    Args = [Server_IP, Server_Port, Port];
	{ok, true} ->
	    {ok, Port} = get_arg(default_port),
	    Args = [Port]
    end,
    
    io:format("Reducing net ticktime to 20 +/- 5 seconds~n"),
    Res = net_kernel:set_net_ticktime(20, 5),
    io:format("Res: ~p~n", [Res]),

    io:format("Establishing Connection: ~p~n", [Args]),

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => connect,
	       start => {connect, start_link, Args},
	       restart => transient,
	       shutdown => 5000,
	       type => worker,
	       modules => [connect]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_arg(master) ->
    case application:get_env(connect, master) of
	{ok, true} ->
	    io:format("Starting as Master~n"),
	    {ok, true};
	_Other ->
	    io:format("Starting as Slave~n"),
	    {ok, false}
    end;

%% get_arg(server_ip) ->
%%     case init:get_argument(server_ip) of
%% 	{ok, [[IP]]} ->
%% 	    inet:parse_address(IP);
%% 	_Error ->
%% 	    io:format("IP of Master Server must be specified with -server_ip~n"),
%% 	    inet:parse_address("0.0.0.0")
%%     end;

%% get_arg(server_port) ->
%%     case init:get_argument(server_port) of
%% 	{ok, [[Port]]} ->
%% 	    {ok, list_to_integer(Port)};
%% 	_Error ->
%% 	    io:format("Port of Master Server must be specified with -server_port~n"),
%% 	    {ok, 4990}
%%     end;

get_arg(default_port) ->
    case application:get_env(connect, default_port) of
	{ok, Port} when is_list(Port) ->
	    {ok, list_to_integer(Port)};
	{ok, Port} ->
	    {ok, Port};
	_Error ->
	    io:format("Starting on default port 4010~n"),
	    {ok, 4010}
    end;

get_arg(master_info) ->
    case application:get_env(connect, master_info) of
	{ok, {IP, Port}} ->
	    {ok, IP2} = inet:parse_address(IP),
	    {ok, {IP2, list_to_integer(Port)}};
	Other ->
	    io:format("Error getting master info: ~p~n", [Other]),
	    {error, not_found}
    end.

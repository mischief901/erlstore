%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%% Defines the logic for starting the Proxy supervisor and server.
%%% @end
%%% Created : 23 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(master_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
%% Maybe change start to take a list of servers to connect to or 
%% just Master and Backup servers.
%%


start(_StartType, _StartArgs) ->
    io:format("Starting Master application~n"),
    master_store:init(),
    io:format("Master Store Initialized~n"),
    case master_delay_start:start_link() of
	{ok, Pid} ->
	    io:format("Master Supervisor started~n"),
	    {ok, Pid};
	Error ->
	    io:format("Master Supervisor failed: ~p~n", [Error])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
%% Maybe save current state to disc here. Probably not do that here though.
%% 
%% TODO: Stop backup proxy app on Master Server if this exits normally.

stop(_State) ->
    net_kernel:stop(),
    application:start(connect),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% None yet. None expected yet either.

%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(backup_app).

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
start(_StartType, _StartArgs) ->
    %% Insert init of ETS table.
    ok = backup_store:init(),
    %% Insert logic to fully connect to Master node and download entire 
    %% existing ETS Table.
    io:format("Backup Store initialized~n"),
    case backup_delay_start:start_link() of
	{ok, Pid} ->
	    io:format("Backup supervisor started~n"),
	    {ok, Pid};
	Error ->
	    Error
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
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

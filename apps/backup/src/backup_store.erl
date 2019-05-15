%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%  Functions to store file information for cached access by the client.
%%% @end
%%% Created : 23 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(backup_store).

%% API
-export([
	 init/0,
	 insert/2,
	 delete/1,
	 lookup/1,
	 new/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

-define(TABLE_ID, ?MODULE).

%%--------------------------------------------------------------------
%% @doc 
%% Creates a named ETS table.
%% @spec init() -> ok
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Inserts a new file name into the table with the last modified and list of
%% storage locations specified.
%% @spec insert(File_Name, {Last_Modified, [Storage_Locations]}) -> ok | 
%%                      {error, Reason}
%% @end
%%--------------------------------------------------------------------
insert(Key, Value) ->
    ets:insert(?TABLE_ID, {Key, Value}).
    
%%--------------------------------------------------------------------
%% @doc
%% Deletes the specified file from the Table.
%% @spec delete(File_Name) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
delete(Key) ->
    ets:delete(?TABLE_ID, Key).

%%--------------------------------------------------------------------
%% @doc
%% Returns the Last_Modified time and list of File_Locations for a given File_Name.
%% @spec lookup(File_Name) -> {ok, {Last_Modified, [File_Locations]}} | 
%%                        {error, Reason}
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
	[{Key, Value}] ->
	    {ok, Value};
	[] ->
	    {error, not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new(Key, Value) ->
    case lookup(Key) of
	{ok, _Entry} ->
	    {error, eexist};
	{error, not_found} ->
	    insert(Key, Value),
	    ok
    end.

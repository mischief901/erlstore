%%%-------------------------------------------------------------------
%%% @author Alex Misch <alex@alex-Lenovo>
%%% @copyright (C) 2018, Alex Misch
%%% @doc
%%%  Functions to store file information for cached access by the client.
%%% @end
%%% Created : 23 Mar 2018 by Alex Misch <alex@alex-Lenovo>
%%%-------------------------------------------------------------------
-module(master_store).

%% API
-export([
	 init/0,
	 insert/2,
	 delete/1,
	 lookup/1,
	 update/2,
	 get_all/0
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
    case lookup(Key) of
	{error, not_found} ->
	    ets:insert(file_store, {Key, Value}),
	    ets:insert(?TABLE_ID, {Key, Value});
	{ok, Key} ->
	    {error, duplicate}
    end.
    
%%--------------------------------------------------------------------
%% @doc
%% Deletes the specified file from the Table.
%% @spec delete(File_Name) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------

delete(Key) ->
    ets:delete(?TABLE_ID, Key),
    ets:delete(file_store, Key),
    ok.


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

update(Key, Value) ->
    case lookup(Key) of
	{error, not_found} ->
	    insert(Key, Value);
	{ok, _Val} ->
	    delete(Key),
	    insert(Key, Value)
    end.

get_all() ->
    List = ets:tab2list(?TABLE_ID),
    io:format("file list: ~p~n", [List]),
    List.

%%%===================================================================
%%% Internal functions
%%%===================================================================

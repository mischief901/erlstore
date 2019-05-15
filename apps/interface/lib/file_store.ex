defmodule FileStore do

  def init() do
    :ets.new(__MODULE__, [:named_table, :public, :set])
    case :ets.info(:master_store) do
      :undefined ->
	:ok
	
	_exists ->
	list = :ets.tab2list(:master_store)
	for {file_name, {timestamp, _, _}} <- list do
	    :io.format("file_name: ~p~n", [file_name])
	    insert(:erlang.list_to_binary(file_name), timestamp)
	end
    end
  end

  def insert(key, value) do
    :ets.insert(__MODULE__, {key, value})
  end

  def delete(key) do
    :ets.delete(__MODULE__, key)
  end

  def lookup(key) do
    case :ets.lookup(__MODULE__, key) do
      [{_key, value}] ->
	{:ok, value}
      [] ->
	{:error, :not_found}
    end
  end

  def get_all() do
    :ets.tab2list(__MODULE__)
  end

  def update_table_from({master_IP, master_Port}) do
    Supervisor.start_child(:download_sup, {master_IP, master_Port})
        
    files = :download.get_all_files
    for {file, values} <- files, do: insert(file, values)
    :ok
  end
  
end

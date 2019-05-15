defmodule InterfaceWeb.DocController do
  use InterfaceWeb, :controller

  ## Displays all the files on the servers.
  def index(conn, _params) do
    ## Get updates list from master
    ## pass list along to render for displaying it all
    
    file_info = FileStore.get_all
    IO.inspect file_info
    
    render conn, "index.html", files: file_info
  end

  ## Redirects to a new page to upload the file.
  def new(conn, _params) do
    render conn, "new_doc.html"
  end

  ## Shows specific meta data for a file.
  ## Last update, Update by, size, etc.
  def show(conn, %{"id" => file_name}) do
    ## Load info from ets table for file
    render conn, "file_info.html", file_name: file_name
  end

  ## Uploads a new file to the servers.
  def create(conn, %{"id" => file_name,
      "file" => file_content}) do
    ## Initiate file transfer with servers
    ## change 1 to timestamp

    FileStore.insert(file_name, 1)

    :io.format("File_Name: ~p~n", [file_name])
    list = :master_store.get_all

    :io.format("master_store: ~p~n", [list])
    
    path = Map.get(file_content, :path, "/")
    
    :ok = :download.new_file(file_name, "#{path}", :filelib.file_size(path))
    
    conn
    |> put_flash(:info, "#{file_name} created.")
    |> render("index.html", files: FileStore.get_all)
    ## go back to index page
  end

  ## uploads a file to the servers to replace the existing file.
  def update(conn, %{"id" => file_name,
      "file" => file_content}) do
    ## upload file to proxy
    ## check that file exists on master and get primary
    ## transfer file to primary
    ## update timestamp in FileStore
    
    path = Map.get(file_content, :path, "/")    

    :download.update_file(file_name,
      "#{path}")
    
    conn
    |> put_flash(:info, "#{file_name} updated.")
    |> render("index.html", files: FileStore.get_all)
    ## go back to index page
  end

  ## Deletes the file from the servers
  def delete(conn, %{"id" => file_name}) do
    ## send message to delete file_name
    :download.delete_file(file_name)
    
    conn
    |> put_flash(:info, "#{file_name} deleted.")
    |> render("index.html")
    ## go back to index page
  end

  ## Starts the download process for the file.
  ## Maybe add field for download type.
  def download(conn, %{"id" => file_name}) do
    ## initiate ftp to client from server
    IO.inspect "Downloading file: #{file_name}"
    IO.inspect "from: #{File.cwd!()}"

    :download.get_file(file_name)

    path = :transport.get_file_path(file_name)

    conn
    |> put_resp_header("content-disposition", "attachment; filename=\"#{File.cwd!}/#{path}\"")
    |> send_file(200, "#{File.cwd!}/#{path}")
    
  end

end

defmodule InterfaceWeb.PageController do
  use InterfaceWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end

  def master(conn, params) do

    ip = params["master_ip"]
    port = params["master_port"]

    :download.add_master({ip, port})
    
    :ets.insert(:master_loc, {ip, port})
    
    conn
    |> put_flash(:info, "Master_Info Updated.")
    |> render("index.html")
  end

end

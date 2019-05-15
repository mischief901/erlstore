defmodule InterfaceWeb.Router do
  use InterfaceWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", InterfaceWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    patch "/", PageController, :master
    
    resources "/docs", DocController, except: [:edit]
    ## the download command replaces the typical edit request
    get "/docs/:id/download", DocController, :download
    
  end

  # Other scopes may use custom stacks.
  # scope "/api", InterfaceWeb do
  #   pipe_through :api
  # end
end

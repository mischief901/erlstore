defmodule Interface.Application do
  use Application

  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec
    IO.puts "Starting Web Interface"
    
    # Define workers and child supervisors to be supervised
    children = [
      # Start the endpoint when the application starts
      supervisor(InterfaceWeb.Endpoint, []),
      supervisor(:download_sup, []),
      # Start your own worker by calling: Interface.Worker.start_link(arg1, arg2, arg3)
      # worker(Interface.Worker, [arg1, arg2, arg3]),
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Interface.Supervisor]
    FileStore.init
    :ets.new(:master_loc, [:named_table, :set, :public])
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    InterfaceWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

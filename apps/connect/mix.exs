Code.require_file "mix.exs", "../.."

defmodule Connect.App.Mixfile do
  use Mix.Project

  def project do
    [
      app: :connect,
      version: "0.1.0",
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:sasl],
      mod: {:connect_app, []},
      included_applications: [:master, :backup, :interface, :file]
    ]
  end

  defp deps do
    []
  end
  
end

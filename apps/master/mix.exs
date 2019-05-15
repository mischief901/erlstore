Code.require_file "mix.exs", "../.."

defmodule Master.App.Mixfile do
  use Mix.Project

  def project do
    [
      app: :master,
      version: "0.1.0"
    ]
  end

  def application do
    [
      mod: {:master_app, []}
    ]
  end
  
end

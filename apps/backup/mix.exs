Code.require_file "mix.exs", "../.."

defmodule Backup.App.Mixfile do
  use Mix.Project

  def project do
    [
      app: :backup,
      version: "0.1.0",
      mod: {:backup_app, []}
    ]
  end

  def application do
    [
      mod: {:backup_app, []}
    ]
  end
  
end

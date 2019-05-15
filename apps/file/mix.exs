Code.require_file "mix.exs", "../.."

defmodule File.App.Mixfile do
  use Mix.Project

  def project do
    [
      app: :file,
      version: "0.1.0",
      mod: {:file_app, []}
    ]
  end

  def application do
    [
      mod: {:file_app, []}
    ]
  end
end

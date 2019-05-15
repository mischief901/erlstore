defmodule ErlStore.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      #start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      applications: [
	:connect,
	:interface,
	:edeliver,
	:runtime_tools,
	:observer
      ]
    ]
  end
  
  # Dependencies listed here are available only for this
  # project and cannot be accessed from applications inside
  # the apps folder.
  #
  # Run "mix help deps" for examples and options.
  defp deps do
    [
      {:conform, "~> 2.2"},
      {:edeliver, "~> 1.4.5"},
      {:distillery, "~> 1.5.0", warn_missing: false}
    ]
  end
end

# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Configures the endpoint
config :interface, InterfaceWeb.Endpoint,
  http: [port: 4000],
  url: [host: "localhost"],
  server: true,
  code_reloader: false,
  secret_key_base: "/K+vEuq1qfeCWL3XkQ4UJZl5/Vz5++uU+eCvzd47uBOUST8elCl6im4bC7nBZHa/",
  render_errors: [view: InterfaceWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Interface.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"

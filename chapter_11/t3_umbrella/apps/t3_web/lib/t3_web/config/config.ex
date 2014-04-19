defmodule T3Web.Config do
  use Phoenix.Config.App

  config :router, port: System.get_env("PORT")

  config :plugs, code_reload: true

  config :logger, level: :error
end



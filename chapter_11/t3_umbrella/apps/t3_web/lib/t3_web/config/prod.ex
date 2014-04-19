defmodule T3Web.Config.Prod do
  use T3Web.Config

  config :router, port: System.get_env("PORT")

  config :plugs, code_reload: false

  config :logger, level: :error
end



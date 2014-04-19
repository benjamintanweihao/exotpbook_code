defmodule T3Web.Config.Dev do
  use T3Web.Config

  config :router, port: 4000,
                  ssl: false

  config :plugs, code_reload: true

  config :logger, level: :debug
end



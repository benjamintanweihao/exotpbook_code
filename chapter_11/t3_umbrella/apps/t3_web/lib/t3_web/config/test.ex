defmodule T3Web.Config.Test do
  use T3Web.Config

  config :router, port: 4001,
                  ssl: false

  config :plugs, code_reload: true

  config :logger, level: :debug
end



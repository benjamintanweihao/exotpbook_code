defmodule T3Web.Router do
  use Phoenix.Router,
  dispatch: [
    { :_, [
        {"/ws/:game_id", T3Web.WebSocketHandler, [] },
        {:_, Plug.Adapters.Cowboy.Handler, { __MODULE__, [] }}
    ]}
  ]

  plug :reload

  get  "/games",T3Web.Controllers.Games, :index
  post "/game", T3Web.Controllers.Games, :create
  post "/games/:game_id/join", T3Web.Controllers.Games, :join

  get "/", T3Web.Controllers.Pages, :index
  
  def start_link do
    start
  end
  
  def init([]) do
  end

  def reload(conn, []) do
    IO.puts "IN RELOAD"
    if T3Web.Config.plugs[:code_reload] == true do
      IO.puts "RELOADING!"
      Mix.Task.reenable("compile.elixir")
    end
    conn
  end

	
end

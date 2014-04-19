defmodule T3Web.Controllers.Games do
  use Phoenix.Controller

  def index(conn) do
    response = T3.list_games 
               |> Enum.map fn game -> [ game_id: game.id ] end

    json conn, JSON.encode! response 
  end

  def create(conn) do
    IO.inspect conn.params
    response = T3.start_game 
               |> T3.join(conn.params["player_id"] |> binary_to_integer)

    json conn, JSON.encode! response 
  end

  def join(conn) do
    response = conn.params["game_id"] 
               |> T3.join(conn.params["player_id"] |> binary_to_integer)

    json conn, JSON.encode! response
  end

end

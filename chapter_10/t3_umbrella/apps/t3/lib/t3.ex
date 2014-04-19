defmodule T3 do
  use Application.Behaviour

  #######
  # API #
  #######

  def start_game do
    { :ok, pid } = T3.Supervisor.start_game
    T3.GameServer.game_id(pid) 
  end

  def join(game_id, player_id) do
    game_id 
      |> T3.DBServer.get_game_pid
      |> T3.GameServer.join(player_id)
  end

  def mark(game_id, player_id, side, pos) do
    game_id 
      |> T3.DBServer.get_game_pid
      |> T3.GameServer.mark(player_id, side, pos)
  end

  def print_board(game_id) do
    game_id 
      |> T3.DBServer.get_game_pid
      |> T3.GameServer.print_board
  end

  def stop(game_id) do
    game_id 
      |> T3.DBServer.get_game_pid
      |> T3.GameServer.stop
  end

  def list_games do
    T3.DBServer.list_games
  end

  #############
  # Callbacks #
  #############

  def start(_type, _args) do
    T3.SuperSupervisor.start_link
  end

end

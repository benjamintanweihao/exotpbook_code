defmodule T3Web.WebSocketHandler do
  @behaviour :cowboy_websocket_handler

  defrecord State, game_id: nil

  def init({:tcp, :http}, _req, _opts) do
    {:upgrade, :protocol, :cowboy_websocket}
  end

  def websocket_init(_transport_name, req, _opts) do
    {game_id, _req_2} = :cowboy_req.binding(:game_id, req)
    game_id |> subscribe
    {:ok, req, State.new(game_id: game_id)}
  end

  def websocket_handle({:text, msg}, req, state) do
    json      = JSON.decode! msg
    game_id   = json["game_id"]
    side      = json["side"] |> binary_to_atom
    player_id = json["player_id"] |> binary_to_integer
    pos       = json["pos"] |> binary_to_integer

    response = T3.mark(game_id, side, player_id, pos) |> JSON.encode!

    # Send message to all connected clients.
    response |> broadcast(game_id)
    
    {:reply, {:text, response}, req, state}
  end

  # A callback from gproc to broadcast msg to all clients of game_id
  def websocket_info({game_id, msg}, req, State[game_id: game_id] = state) do
    {:reply, {:text, msg}, req, state}
  end

  def websocket_info({:timeout, _ref, msg}, req, state) do
    {:reply, {:text, msg}, req, state}
  end

  def websocket_info(_info, req, state) do
    {:ok, req, state}
  end

  def websocket_terminate(_reason, _req, _state) do
    :ok
  end

  #####################
  # Private functions #
  #####################

  # NOTE: I think a good question here to ask is:
  # How in the world does gproc know who to register? In other
  # words, how did gproc know it is *this* process?
  # Looking at the source:
  # 
  #   https://github.com/uwiger/gproc/blob/master/src/gproc.erl#L2017
  # 
  # and also recalling that handle_call also takes in a 'from'!
  defp subscribe(game_id) do
    :gproc.reg {:p, :l, game_id}
  end

  defp broadcast(msg, game_id) do
    :gproc.send {:p, :l, game_id}, {game_id, msg}
  end
end

defmodule T3.GameServer do
  use GenServer.Behaviour

  defrecord State, game_id: nil, x_id: nil, o_id: nil, next: :x, 
                   board: nil, result: :empty

  #######
  # API #
  #######

  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

	def join(server_pid, p_id) do
	  :gen_server.call(server_pid, {:join, p_id})
	end

	def mark(server_pid, side, p_id, pos) do
	  :gen_server.call(server_pid, {:mark, side, p_id, pos})
	end

  def game_id(server_pid) do
    :gen_server.call(server_pid, :game_id)
  end

	def print_board(server_pid) do
	  :gen_server.cast(server_pid, :print_board)
	end

	def stop(server_pid) do
	  :gen_server.cast(server_pid, :stop)
	end

  #############
  # Callbacks #
  #############

  def init([]) do
    game_id = T3.Utils.SecureRandom.hex |> T3.DBServer.create_game(self)
    { :ok, State.new(game_id: game_id, board: new_board) }
  end

  # Player X join
  def handle_call({:join, p_id}, _from, State[x_id: nil] = state) do
	  new_state = state.update(x_id: p_id)
    { :reply, response(:success, new_state, :waiting), new_state }
  end

  # Player O join
  def handle_call({:join, p_id}, _from, State[x_id: x_id, o_id: nil] = state) 
    when is_number(x_id) do

    new_state = state.update(o_id: p_id)
    { :reply, response(:success, new_state, :in_progress), new_state }
	end

  # Game Full
  def handle_call({:join, _p_id}, _from, state) do
    { :reply, response(:error, state, :game_full), state }
  end

  def handle_call({:mark, :x, p_id, pos}, 
                   _from, 
                   State[x_id: p_id, next: :x] = state) do
                     
    case mark_board(:x, pos, state.board) do
      {:ok, new_board, new_result} ->

        new_state = state.update(next: next(state.next), 
                                board: new_board, 
                               result: new_result)

        { :reply, response(:success, new_state, :in_progress), new_state }

      :error ->
        { :reply, response(:error, state, :invalid_move), state }
    end
  end

  def handle_call({:mark, :o, p_id, pos}, 
                   _from, 
                   State[o_id: p_id, next: :o, result: :in_progress] = state) do
                     
    case mark_board(:o, pos, state.board) do
      {:ok, new_board, new_result} ->

        new_state = state.update(next: next(state.next), 
                                board: new_board, 
                               result: new_result)
        
        { :reply, response(:success, new_state, :in_progress), new_state }

      :error ->

        { :reply, response(:error, state, :invalid_move), state }
    end
  end

  def handle_call({:mark, _side, _p_id, _pos}, _from, state) do
    { :reply, response(:error, state, :invalid_move), state }
  end

  def handle_call(:game_id, _from, state) do
    { :reply, state.game_id, state }
  end

  def handle_cast(:print_board, State[board: board] = state) do
    IO.puts " #{cell(board, 1)} | #{cell(board, 2)} | #{cell(board, 3)}"
    IO.puts "---|---|---"
    IO.puts " #{cell(board, 4)} | #{cell(board, 5)} | #{cell(board, 6)}"
    IO.puts "---|---|---"
    IO.puts " #{cell(board, 7)} | #{cell(board, 8)} | #{cell(board, 9)}"
    { :noreply, state }
  end

  def handle_cast(:stop, state) do
    { :stop, :normal, state }
  end
	
	def handle_info(msg, state) do
    IO.inspect msg
	  { :noreply, state } 
	end

  def terminate(reason, state) do
    IO.puts "Server terminated due to: #{inspect reason}. State is #{inspect state}"
	  T3.DBServer.remove_game(state.game_id)
  end
  
  ######################
  # Internal Functions #
  ######################

  defp new_board, do: nil |> Tuple.duplicate 9

  defp mark_board(side, pos, board) do
    case elem(board, pos-1) do
      nil -> 
        new_board = set_elem(board, pos-1, side)
        result    = check_board(new_board)
        print_board(self)
        {:ok, new_board, result}
      _ ->
        print_board(self)
        :error
    end
  end

  defp check_board(board) do
    case board do
      { :x, :x, :x,
        _ , _ , _ ,
        _ , _ , _ } -> :x_win

      { _ , _ , _ ,
        :x, :x, :x,
        _ , _ , _ } -> :x_win

      { _ , _ , _ ,
        _ , _ , _ ,
        :x, :x, :x} -> :x_win

      { :x, _ , _ ,
        :x, _ , _ ,
        :x, _ , _ } -> :x_win

      { _ , :x, _ ,
        _ , :x, _ ,
        _ , :x, _ } -> :x_win

      { _ , _ , :x,
        _ , _ , :x,
        _ , _ , :x} -> :x_win

      { :x, _ , _ ,
        _ , :x, _ ,
        _ , _ , :x} -> :x_win

      { _ , _ , :x,
        _ , :x, _ ,
        :x, _ , _ } -> :x_win

      { :o, :o, :o,
        _ , _ , _ ,
        _ , _ , _ } -> :o_win

      { _ , _ , _ ,
        :o, :o, :o,
        _ , _ , _ } -> :o_win

      { _ , _ , _ ,
        _ , _ , _ ,
        :o, :o, :o} -> :o_win

      { :o, _ , _ ,
        :o, _ , _ ,
        :o, _ , _ } -> :o_win

      { _ , :o, _ ,
        _ , :o, _ ,
        _ , :o, _ } -> :o_win

      { _ , _ , :o,
        _ , _ , :o,
        _ , _ , :o} -> :o_win

      { :o, _ , _ ,
        _ , :o, _ ,
        _ , _ , :o} -> :o_win

      { _ , _ , :o,
        _ , :o, _ ,
        :o, _ , _ } -> :o_win

      { a, b, c, 
        d, e, f, 
        g, h, i } when a and b and c 
                   and d and e and f 
                   and g and h and i -> :draw

      _ -> :in_progress

    end
  end

  defp next(:x), do: :o
  defp next(:o), do: :x

  defp cell(board, pos) do
    case elem(board,pos-1) do
      nil ->
        " "
      mark -> mark
    end
  end

  defp response(status, State[game_id: gid, x_id: x_id, o_id: o_id, next: next, board: board, result: result], message) do
    data = [game_id: gid, x_id: x_id, o_id: o_id, next: next, board: tuple_to_list(board), result: result]
    HashDict.new(status: status, data: data, message: message)
  end
  
end

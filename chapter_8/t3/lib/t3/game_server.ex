defmodule T3.GameServer do
  use GenServer.Behaviour

  defrecord State, x_id: nil, o_id: nil, next: :x, board: nil, result: :empty

  #######
  # API #
  #######

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def join(p_id) do
    :gen_server.call(__MODULE__, {:join, p_id})
  end

  def mark(side, p_id, pos) do
    :gen_server.call(__MODULE__, {:mark, side, p_id, pos})
  end

  def print_board do
    :gen_server.cast(__MODULE__, :print_board)
  end

  def stop do
    :gen_server.cast(__MODULE__, :stop)
  end

  #############
  # Callbacks #
  #############

  def init([]) do
    { :ok, State.new(board: new_board) }
  end

  # Player X join
  def handle_call({:join, p_id}, _from, State[x_id: nil] = state) do
    { :reply, :waiting, state.update(x_id: p_id) }
  end

  # Player O join
  def handle_call({:join, p_id}, _from, State[x_id: x_id, o_id: nil] = state) 
    when is_pid(x_id) do
    { :reply, :in_progress, state.update(o_id: p_id) }
  end

  # Game Full
  def handle_call({:join, _p_id}, _from, state) do
    { :reply, :game_full, state }
  end

  def handle_call({:mark, :x, p_id, pos}, 
                   _from, 
                   State[x_id: p_id, next: :x] = state) do
                     
    case mark_board(:x, pos, state.board) do
      {:ok, new_board, new_result} ->
        
        { :reply, new_result, state.update(next: next(state.next), 
                                         board: new_board, 
                                        result: new_result) }
      :error ->
        { :reply, :invalid_move, state }
    end
  end

  def handle_call({:mark, :o, p_id, pos}, 
                   _from, 
                   State[o_id: p_id, next: :o, result: :in_progress] = state) do
                     
    case mark_board(:o, pos, state.board) do
      {:ok, new_board, new_result} ->
        
        { :reply, :ok, state.update(next: next(state.next), 
                                   board: new_board, 
                                  result: new_result) }
      :error ->
        { :reply, :invalid_move, state }
    end
  end

  def handle_call({:mark, _side, _p_id, _pos}, _from, state) do
    { :reply, :invalid_move, state }
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

  def terminate(reason, state) do
    IO.puts "Server terminated due to: #{reason}. State is #{inspect state}"
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
        print_board
        {:ok, new_board, result}
      _ ->
        print_board
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
  
end

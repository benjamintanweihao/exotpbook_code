defmodule T3.DBServer do
  use GenServer.Behaviour

  defrecord Game, id: nil, pid: nil, state: :empty

  #######
  # API #
  #######

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def create_game(game_id, game_pid) do
    :gen_server.call(__MODULE__, {:create_game, game_id, game_pid})
  end

  def get_game_pid(game_id) do
    :gen_server.call(__MODULE__, {:get_game_pid, game_id})
  end

  def list_games do
    :gen_server.call(__MODULE__, :list_games)
  end

  def remove_game(game_id) do
    :gen_server.cast(__MODULE__, {:remove_game, game_id})
  end

  def stop do
    :gen_server.cast(__MODULE__, :stop)
  end
	
  #############
  # Callbacks #
  #############
	
  def init(_args) do
    setup
	 { :ok, [] }
  end

  def handle_call({:create_game, game_id, game_pid}, _from, state) do
    :ets.insert table_name, Game.new(id: game_id, pid: game_pid)
    { :reply, game_id, state }
  end

  def handle_call({:get_game_pid, game_id}, _from, state) do
    reply = case :ets.lookup(table_name, game_id) |> List.first do
      nil ->
        nil 
      result ->
        result.pid
    end
    {:reply, reply, state}
  end

  def handle_call(:list_games, _from, state) do
    {:reply, :ets.tab2list(table_name), state}
  end

  def handle_cast({:remove_game, game_id}, state) do
    :ets.delete(table_name, game_id)
    {:noreply, state}
  end

  def handle_cast(:stop, state) do
    :ets.delete(table_name)
    {:stop, :ok, state}
  end
	
	###################### 
	# Internal Functions #
  ######################
	
	def setup do
	  :ets.new(table_name, 
	    [:named_table, :set, :public,
	      {:keypos, Game.__record__(:index, :id) + 1}
	    ])
	  |> :ets.info
	end

	defp table_name do
	  :games
	end
end
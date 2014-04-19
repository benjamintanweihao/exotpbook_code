defmodule T3.Supervisor do
  use Supervisor.Behaviour

  #######
  # API #
  #######

  def start_link do
    # We are only using one supervisor per node. So giving it a name is fine.
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def start_game do
    :supervisor.start_child(__MODULE__, [])
  end
  
  #############
  # Callbacks #
  #############

  def init([]) do
    # By default, T3.GameServer.start_link would be called. But, 
    # since we are using :simple_one_for_one, no child is started.
    
    # Note we are using restart: :temporary. When a game session dies,
    # we +never+ restart it.
    children = [ worker(T3.GameServer, [], restart: :temporary) ]

    supervise(children, strategy: :simple_one_for_one)
  end
end

defmodule T3.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    children = [
      worker(T3.GameServer, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end

defmodule T3.SuperSupervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    children = [
      worker(T3.DBServer, []),
      supervisor(T3.Supervisor, [])
    ]

    supervise(children, strategy: :one_for_all)
  end
end
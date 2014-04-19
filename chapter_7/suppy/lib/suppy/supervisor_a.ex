defmodule Suppy.SupervisorA do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    IO.puts "Starting #{__MODULE__} (#{inspect self})..."

    children = [
      supervisor(Suppy.SupervisorB, []),
      supervisor(Suppy.SupervisorC, []),
    ]

    supervise(children, strategy: :one_for_one)
  end

  def crash do
    Process.exit(Process.whereis(__MODULE__), :kill)
  end

end

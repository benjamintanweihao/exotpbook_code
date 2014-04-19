defmodule Suppy.SupervisorC do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def start_child do
    :supervisor.start_child(__MODULE__, [])
  end

  def init(_args) do
    IO.puts "Starting #{__MODULE__} (#{inspect self})..."

    children = [
      worker(Suppy.WorkerC, [])
    ]

    supervise(children, strategy: :simple_one_for_one)
  end

  def crash do
    Process.exit(Process.whereis(__MODULE__), :kill)
  end

end

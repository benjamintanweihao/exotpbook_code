defmodule Acky.Server do
  use GenServer.Behaviour

  #######
  # API #
  #######

  def start_link do
    :gen_server.start_link(__MODULE__, HashDict.new, [])
  end

  def compute(server_pid, {m, n}) do
    :gen_server.call(server_pid, {:compute, {m, n}}, :infinity)
  end

  def view_cache(server_pid) do
    :gen_server.call(server_pid, :view_cache)
  end


  def stop(server_pid) do
    :gen_server.cast(server_pid, :stop)
  end

  #############
  # Callbacks #
  #############

  def init(cache) do
    {:ok, cache}
  end

  def handle_call(:view_cache, _from, cache) do
    {:reply, cache, cache}
  end

  def handle_call({:compute, {m, n}}, _from, cache) do
    case HashDict.fetch(cache, {m, n}) do
      {:ok, result} ->
        response = result
      :error ->
        response = ackermann(m, n)
        cache = cache |> HashDict.put({m,n}, response)
    end
    {:reply, response, cache}
  end


  def handle_cast(:stop, cache) do
    {:stop, :normal, cache}
  end

  ######################
  # Internal Functions #
  ######################

  defp ackermann(0, n), do: n + 1
  defp ackermann(m, 0), do: ackermann(m-1, 1)
  defp ackermann(m, n), do: ackermann(m-1, ackermann(m,n-1))
end

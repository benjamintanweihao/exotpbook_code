defmodule DistributingWork do
  def process_work([], [], _, results) do
    IO.puts inspect(results, char_lists: :as_lists)
  end

  def process_work(work, active, passive, results) 
    when work == [] or passive == [] do
    receive do
      { worker, result } ->
        process_work(work, List.delete(active, worker), [worker | passive], 
                     [result | results])
    end
  end

  def process_work([{m,n}|rest], active, [worker | passive], results) do
    worker |> send({ self, {m, n} })
    process_work(rest, [worker | active], passive, results)
  end

  def worker do
    receive do
      { pid, {m, n} } -> 
        pid |> send({ self, sequential({m, n}) })
        worker
    end
  end

  def sequential({m, n}), do: ackermann(m, n)

  def ackermann(0, n), do: n + 1
  def ackermann(m, 0), do: ackermann(m-1, 1)
  def ackermann(m, n), do: ackermann(m-1, ackermann(m,n-1))

  def run(n) do
    workers = 1..n |> Enum.map fn _ -> spawn(__MODULE__, :worker, []) end
    work    = [{4,1}, {4,1}, {4,1}, {4,1}]
    process_work(work, [], workers, [])
  end

end

s = :erlang.now
DistributingWork.run(1)
e = :erlang.now
IO.inspect :timer.now_diff(e, s) / (1000 * 1000)

s = :erlang.now
DistributingWork.run(2)
e = :erlang.now
IO.inspect :timer.now_diff(e, s) / (1000 * 1000)
  
s = :erlang.now
DistributingWork.run(3)
e = :erlang.now
IO.inspect :timer.now_diff(e, s) / (1000 * 1000)
  
s = :erlang.now
DistributingWork.run(4)
e = :erlang.now
IO.inspect :timer.now_diff(e, s) / (1000 * 1000)

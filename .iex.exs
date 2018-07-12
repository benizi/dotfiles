defmodule Benizi do
  defmodule History do
    alias IEx.History, as: Hist
    use Task, restart: :transient

    def start_link(_arg), do: Task.start_link(__MODULE__, :run, [])

    def run(_arg), do: loop()

    defp loop() do
      case Application.get_all_env(:iex) do
        [] ->
          IO.puts "WAITING"
          Process.sleep(100)
          loop()

        _ ->
          IO.puts "STARTED"
          h = :iex_history
          Process.get(h) |> IO.inspect
          Enum.reduce(1..10, Hist.init, fn n,a -> Hist.append(a,{n,'\n',nil},-1) end)
          |> (fn x -> IO.inspect x ; x end).()
          |> Process.put(h)
      end
    end
  end

  def start() do
    {:ok, pid} = Task.Supervisor.start_link()
    task = Task.Supervisor.start_child(pid, History, :run, [nil])
    Process.put(:benizi_start, {:ok, [pid: pid, task: task]})
  end
end
Benizi.start
#{:ok, supervisor} = Task.Supervisor.start_link()
#hist_task = Task.Supervisor.async
#Process.get(:iex_history) |> IO.inspect
IEx.configure(history_size: -1)

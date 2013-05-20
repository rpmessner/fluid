ExUnit.start

defmodule LocalState do
  use GenServer.Behaviour

  def start do
    :gen_server.start({:local, __MODULE__}, __MODULE__, 0, [])
  end

  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  def increment do
    :gen_server.cast(__MODULE__, :increment)
  end

  def reset do
    :gen_server.cast(__MODULE__, :reset)
  end

  def get do
    :gen_server.call(__MODULE__, :get)
  end

  def handle_call(:stop, _from, state) do
    { :stop, :normal, :ok, state }
  end

  def handle_call(:get, _from, state) do
    { :reply, state, state }
  end

  def handle_cast(:reset, _) do
    { :noreply, 0 }
  end

  def handle_cast(:increment, state) do
    state = state + 1
    { :noreply, state }
  end

end

defmodule Liquescent.Registers do
  use GenServer

  defp default_tags do
    [defaultcontent: { Liquescent.Default,  Liquescent.Tag },
     continue:       { Liquescent.Continue, Liquescent.Tag },
     extended:       { Liquescent.Extends,  Liquescent.Block },
     comment:        { Liquescent.Comment,  Liquescent.Block },
     include:        { Liquescent.Include,  Liquescent.Tag },
     assign:         { Liquescent.Assign,   Liquescent.Tag },
     block:          { Liquescent.Inherit,  Liquescent.Block },
     break:          { Liquescent.Break,    Liquescent.Tag },
     elsif:          { Liquescent.ElseIf,   Liquescent.Tag },
     else:           { Liquescent.Else,     Liquescent.Tag },
     case:           { Liquescent.Case,     Liquescent.Block },
     when:           { Liquescent.When,     Liquescent.Tag },
     for:            { Liquescent.ForElse,  Liquescent.Block },
     if:             { Liquescent.IfElse,   Liquescent.Block },
     unless:         { Liquescent.Unless,   Liquescent.Block},
     raw:            { Liquescent.Raw,      Liquescent.Block},
     increment:      { Liquescent.Increment, Liquescent.Tag},
     decrement:      { Liquescent.Decrement, Liquescent.Tag},
     capture:        { Liquescent.Capture, Liquescent.Block}]
  end

  def handle_cast({ :register, <<name::binary>>, module, tag }, dict) do
    { :noreply, dict |> Dict.put(name |> String.to_atom, { module, tag }) }
  end

  def handle_cast(:clear, _dict) do
    { :noreply, default_tags }
  end

  def handle_call({ :lookup, name }, _from, dict) when is_atom(name) do
    result = Dict.get(dict, name)
    { :reply, result, dict }
  end

  def handle_call({ :lookup, <<name::binary>> }, _from, dict) do
    result = Dict.get(dict, name |> String.to_atom)
    { :reply, result, dict }
  end

  def handle_call(:stop, _from, dict) do
    { :stop, :normal, :ok, dict }
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def start do
    :gen_server.start({ :local, __MODULE__ }, __MODULE__, default_tags, [])
  end

  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  def clear do
    :gen_server.call(__MODULE__, :clear)
  end

  def register(name, module, type) do
    :gen_server.cast(__MODULE__, { :register, name, module, type })
  end

  def lookup(name) do
    :gen_server.call(__MODULE__, { :lookup, name })
  end
end

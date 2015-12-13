defmodule Liquescent.Registers do
  use GenServer

  defp default_tags do
    [defaultcontent: { Liquescent.Default,  Liquescent.Tags },
     continue:       { Liquescent.Continue, Liquescent.Tags },
     extended:       { Liquescent.Extends,  Liquescent.Blocks },
     comment:        { Liquescent.Comment,  Liquescent.Blocks },
     include:        { Liquescent.Include,  Liquescent.Tags },
     assign:         { Liquescent.Assign,   Liquescent.Tags },
     block:          { Liquescent.Inherit,  Liquescent.Blocks },
     break:          { Liquescent.Break,    Liquescent.Tags },
     elsif:          { Liquescent.ElseIf,   Liquescent.Tags },
     else:           { Liquescent.Else,     Liquescent.Tags },
     case:           { Liquescent.Case,     Liquescent.Blocks },
     when:           { Liquescent.When,     Liquescent.Tags },
     for:            { Liquescent.ForElse,  Liquescent.Blocks },
     if:             { Liquescent.IfElse,   Liquescent.Blocks },
     unless:         { Liquescent.Unless,   Liquescent.Blocks},
     raw:            { Liquescent.Raw,      Liquescent.Blocks},
     increment:      { Liquescent.Increment, Liquescent.Tags},
     decrement:      { Liquescent.Decrement, Liquescent.Tags}]
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

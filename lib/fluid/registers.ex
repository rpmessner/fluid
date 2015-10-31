defmodule Fluid.Registers do
  use GenServer

  defp default_tags do
    [defaultcontent: { Fluid.Default,  Fluid.Tags },
     continue:       { Fluid.Continue, Fluid.Tags },
     extended:       { Fluid.Extends,  Fluid.Blocks },
     comment:        { Fluid.Comment,  Fluid.Blocks },
     include:        { Fluid.Include,  Fluid.Tags },
     extends:        { Fluid.Extends,  Fluid.Tags },
     assign:         { Fluid.Assign,   Fluid.Tags },
     block:          { Fluid.Inherit,  Fluid.Blocks },
     break:          { Fluid.Break,    Fluid.Tags },
     elsif:          { Fluid.ElseIf,   Fluid.Tags },
     else:           { Fluid.Else,     Fluid.Tags },
     case:           { Fluid.Case,     Fluid.Blocks },
     when:           { Fluid.When,     Fluid.Tags },
     for:            { Fluid.ForElse,  Fluid.Blocks },
     if:             { Fluid.IfElse,   Fluid.Blocks }]
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

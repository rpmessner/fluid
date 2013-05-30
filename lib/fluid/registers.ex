defmodule Fluid.Registers do
  use GenServer.Behaviour

  defp default_tags do
    [defaultcontent: { Fluid.Default,  Fluid.Tag },
     continue:       { Fluid.Continue, Fluid.Tag },
     extended:       { Fluid.Extends,  Fluid.Block },
     comment:        { Fluid.Comment,  Fluid.Block },
     include:        { Fluid.Include,  Fluid.Tag },
     extends:        { Fluid.Extends,  Fluid.Tag },
     assign:         { Fluid.Assign,   Fluid.Tag },
     block:          { Fluid.Inherit,  Fluid.Block },
     break:          { Fluid.Break,    Fluid.Tag },
     elsif:          { Fluid.ElseIf,   Fluid.Tag },
     else:           { Fluid.Else,     Fluid.Tag },
     case:           { Fluid.Case,     Fluid.Block },
     when:           { Fluid.When,     Fluid.Tag },
     for:            { Fluid.ForElse,  Fluid.Block },
     if:             { Fluid.IfElse,   Fluid.Block }]
  end

  def handle_cast({ :register, <<name::binary>>, module, tag }, dict) do
    { :noreply, dict |> Dict.put(name |> binary_to_atom(:utf8), { module, tag }) }
  end

  def handle_cast(:clear, _dict) do
    { :noreply, default_tags }
  end

  def handle_call({ :lookup, name }, _from, dict) when is_atom(name) do
    result = Dict.get(dict, name)
    { :reply, result, dict }
  end

  def handle_call({ :lookup, <<name::binary>> }, _from, dict) do
    result = Dict.get(dict, name |> binary_to_atom(:utf8))
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

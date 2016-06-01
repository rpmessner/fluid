defmodule Liquid.Registers do
  use GenServer

  defp default_tags do
    %{defaultcontent: { Liquid.Default,  Liquid.Tag },
     continue:       { Liquid.Continue, Liquid.Tag },
     extended:       { Liquid.Extends,  Liquid.Block },
     comment:        { Liquid.Comment,  Liquid.Block },
     include:        { Liquid.Include,  Liquid.Tag },
     assign:         { Liquid.Assign,   Liquid.Tag },
     block:          { Liquid.Inherit,  Liquid.Block },
     break:          { Liquid.Break,    Liquid.Tag },
     elsif:          { Liquid.ElseIf,   Liquid.Tag },
     else:           { Liquid.Else,     Liquid.Tag },
     case:           { Liquid.Case,     Liquid.Block },
     when:           { Liquid.When,     Liquid.Tag },
     for:            { Liquid.ForElse,  Liquid.Block },
     if:             { Liquid.IfElse,   Liquid.Block },
     unless:         { Liquid.Unless,   Liquid.Block},
     raw:            { Liquid.Raw,      Liquid.Block},
     increment:      { Liquid.Increment, Liquid.Tag},
     decrement:      { Liquid.Decrement, Liquid.Tag},
     capture:        { Liquid.Capture, Liquid.Block}}
  end

  def handle_cast({ :register, name, module, tag }, dict) when is_binary(name) do
    { :noreply, dict |> Map.put(name |> String.to_atom, { module, tag }) }
  end

  def handle_cast(:clear, _dict) do
    { :noreply, default_tags }
  end

  def handle_call({ :lookup, name }, _from, dict) when is_atom(name) do
    result = Map.get(dict, name)
    { :reply, result, dict }
  end

  def handle_call({ :lookup, name }, _from, dict) when is_binary(name) do
    result = Map.get(dict, name |> String.to_atom)
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

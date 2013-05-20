defmodule Fluid.Templates do
  use GenServer.Behaviour

  defp default_tags do
    [comment: { Fluid.Comment, Fluid.Block },
     assign:  { Fluid.Assign,  Fluid.Tag },
     elsif:   { Fluid.ElseIf,  Fluid.Tag },
     else:    { Fluid.Else,    Fluid.Tag },
     for:     { Fluid.ForElse, Fluid.Block },
     if:      { Fluid.IfElse,  Fluid.Block }]
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

  def handle_cast({ :register, <<name::binary>>, module, tag }, dict) do
    { :noreply, Dict.put(dict, binary_to_atom(name, :utf8), { module, tag }) }
  end

  def handle_call({ :lookup, name }, _from, dict) when is_atom(name) do
    result = Dict.get(dict, name)
    { :reply, result, dict }
  end

  def handle_call({ :lookup, <<name::binary>> }, _from, dict) do
    result = Dict.get(dict, binary_to_atom(name, :utf8))
    { :reply, result, dict }
  end

  def handle_call(:stop, _from, dict) do
    { :stop, :normal, :ok, dict }
  end

  def handle_cast(:clear, dict) do
    spawn fn ->
      Enum.each dict, fn({ _, { module, _ } }) ->
        purge_module(module)
      end
    end
    { :noreply, default_tags }
  end

  defp purge_module(module) do
    :code.delete(module)
    :code.purge(module)
  end

  def register(name, module, type) do
    :gen_server.cast(__MODULE__, { :register, name, module, type })
  end

  def lookup(name) do
    :gen_server.call(__MODULE__, { :lookup, name })
  end

  alias Fluid.Template, as: Template
  alias Fluid.Render, as: Render
  alias Fluid.Context, as: Context

  def render(Template[]=t), do: render(t, [])
  def render(Template[]=t, assigns) when is_list(assigns) do
    context = Context[assigns: assigns, presets: t.presets]
    Render.render(t, context)
  end

  def render(Template[]=t, Context[]=c), do: Render.render(t, t.presets |> c.presets)

  def parse(<<markup::binary>>, presets//[]) do
    Fluid.Parse.parse(markup, presets)
  end
end

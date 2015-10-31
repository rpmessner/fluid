defmodule Fluid.Templates do
  defstruct root: nil, presets: [], blocks: []
require IEx
  alias Fluid.Templates, as: Templates
  alias Fluid.Render, as: Render
  alias Fluid.Contexts, as: Contexts

  def render(%Templates{}=t), do: render(t, [])
  def render(%Templates{}=t, assigns) when is_list(assigns) do
    IEx.pry
    context = %Contexts{template: t,         assigns: assigns,
                      presets:  t.presets, blocks: t.blocks}
    Render.render(t, context)
  end

  def render(%Templates{}=t, %Contexts{}=c) do
    IEx.pry
    c = t.blocks |> c.blocks
    c = t.presets |> c.presets
    c = t |> c.template
    Render.render(t, c)
  end

  def parse(<<markup::binary>>, presets \\ []) do
    IEx.pry
    Fluid.Parse.parse(markup, %Templates{presets: presets})
  end

end

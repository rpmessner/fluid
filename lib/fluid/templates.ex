defmodule Fluid.Templates do
  defstruct root: nil, presets: [], blocks: []
  alias Fluid.Templates, as: Templates
  alias Fluid.Render, as: Render
  alias Fluid.Contexts, as: Contexts

  def render(%Templates{}=t), do: render(t, [])
  def render(%Templates{}=t, assigns) when is_list(assigns) do
    context = %Contexts{template: t,         assigns: assigns,
                      presets:  t.presets, blocks: t.blocks}
    Render.render(t, context)
  end

  def render(%Templates{}=t, %Contexts{}=c) do
    c = %{c | blocks: t.blocks }
    c = %{c | presets: t.presets }
    c = %{c | template: t }
    Render.render(t, c)
  end

  def parse(<<markup::binary>>, presets \\ []) do
    Fluid.Parse.parse(markup, %Templates{presets: presets})
  end

end

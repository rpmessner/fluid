defmodule Liquescent.Templates do
  defstruct root: nil, presets: [], blocks: []
  alias Liquescent.Templates, as: Templates
  alias Liquescent.Render, as: Render
  alias Liquescent.Contexts, as: Contexts

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

    Liquescent.Parse.parse(markup, %Templates{presets: presets})
  end

end

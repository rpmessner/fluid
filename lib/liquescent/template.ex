defmodule Liquescent.Template do
  defstruct root: nil, presets: [], blocks: []
  alias Liquescent.Template, as: Template
  alias Liquescent.Render, as: Render
  alias Liquescent.Context, as: Context

  def render(%Template{}=t), do: render(t, [])
  def render(%Template{}=t, assigns) when is_list(assigns) do
    context = %Context{template: t,         assigns: assigns,
                      presets:  t.presets, blocks: t.blocks}
    Render.render(t, context)
  end

  def render(%Template{}=t, %Context{}=c) do
    c = %{c | blocks: t.blocks }
    c = %{c | presets: t.presets }
    c = %{c | template: t }
    Render.render(t, c)
  end

  def parse(<<markup::binary>>, presets \\ []) do

    Liquescent.Parse.parse(markup, %Template{presets: presets})
  end

end

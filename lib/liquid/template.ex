defmodule Liquid.Template do
  defstruct root: nil, presets: %{}, blocks: []
  alias Liquid.Template, as: Template
  alias Liquid.Render, as: Render
  alias Liquid.Context, as: Context

  def render(t, c \\ %{})#, do: render(t, %{})
  def render(%Template{}=t, %Context{}=c) do
    c = %{c | blocks: t.blocks }
    c = %{c | presets: t.presets }
    c = %{c | template: t }
    Render.render(t, c)
  end

  def render(%Template{}=t, assigns) when is_map(assigns) do
    context = %Context{template: t,         assigns: assigns,
                      presets:  t.presets, blocks: t.blocks}
    Render.render(t, context)
  end

  def render(_, _) do
    raise Liquid.SyntaxError, message: "You can use only maps/structs to hold context data"
  end

  def parse(<<markup::binary>>, presets \\ %{}) do
    Liquid.Parse.parse(markup, %Template{presets: presets})
  end

end

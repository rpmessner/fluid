defmodule Fluid.Templates do
  alias Fluid.Template, as: Template
  alias Fluid.Render, as: Render
  alias Fluid.Context, as: Context

  def render(Template[]=t), do: render(t, [])
  def render(Template[]=t, assigns) when is_list(assigns) do
    context = Context[template: t,         assigns: assigns,
                      presets:  t.presets, blocks: t.blocks]
    Render.render(t, context)
  end

  def render(Template[]=t, Context[]=c) do
    c = t.blocks |> c.blocks
    c = t.presets |> c.presets
    c = t |> c.template
    Render.render(t, c)
  end

  def parse(<<markup::binary>>, presets//[]) do
    Fluid.Parse.parse(markup, Template[presets: presets])
  end

end

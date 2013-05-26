defmodule Fluid.Templates do
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

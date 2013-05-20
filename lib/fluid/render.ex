defmodule Fluid.Render do
  alias Fluid.Variables, as: Variables
  alias Fluid.Templates, as: Templates

  def render(Fluid.Template[root: root, presets: presets], assigns) do
    { output, assigns } = render([], root, assigns, presets)
    { Enum.join(output), assigns }
  end

  def render(output, <<text::binary>>, assigns, presets) do
    { output ++ [text], assigns }
  end

  def render(output, Fluid.Variable[]=v, assigns, presets) do
    { rendered, assigns } = Variables.lookup(v, assigns, presets)
    { output ++ [rendered], assigns }
  end

  def render(output, Fluid.Tag[name: name]=tag, assigns, presets) do
    { mod, Fluid.Tag } = Templates.lookup(name)
    mod.render(output, tag, assigns, presets)
  end

  def render(output, Fluid.Block[name: name]=block, assigns, presets) do
    case Templates.lookup(name) do
      { mod, Fluid.Block } ->
        mod.render(output, block, assigns, presets)
      nil -> render(output, block.nodelist, assigns, presets)
    end
  end

  def render(output, [h|t], assigns, presets) do
    { output, assigns } = render(output, h, assigns, presets)
    render(output, t, assigns, presets)
  end

  def render(output, [], assigns, presets) do
    { output, assigns }
  end
end

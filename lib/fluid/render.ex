defmodule Fluid.Render do
  alias Fluid.Variables, as: Variables
  alias Fluid.Variable, as: Variable
  alias Fluid.Templates, as: Templates
  alias Fluid.Template, as: Template
  alias Fluid.Registers, as: Registers
  alias Fluid.Context, as: Context
  alias Fluid.Block, as: Block
  alias Fluid.Tag, as: Tag

  def render(%Template{root: root}, %Context{}=context) do
    { output, context } = render([], root, context)
    { :ok, Enum.join(output), context }
  end

  def render(output, [], %Context{}=context), do: { output, context }
  def render(output, [h|t], %Context{}=context) do
    { output, context } = render(output, h, context)
    case context do
      Context[extended: false, break: false, continue: false] -> render(output, t, context)
      _ -> render(output, [], context)
    end
  end

  def render(output, <<text::binary>>, %Context{}=context) do
    { output ++ [text], context }
  end

  def render(output, %Variable{}=v, %Context{}=context) do
    { rendered, context } = Variables.lookup(v, context)
    { output ++ [rendered], context }
  end

  def render(output, Tag[name: name]=tag, %Context{}=context) do
    { mod, Tag } = Registers.lookup(name)
    mod.render(output, tag, context)
  end

  def render(output, Block[name: name]=block, %Context{}=context) do
    case Registers.lookup(name) do
      { mod, Block } -> mod.render(output, block, context)
      nil -> render(output, block.nodelist, context)
    end
  end

end

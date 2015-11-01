defmodule Fluid.Render do
  alias Fluid.Variables, as: Variables
  alias Fluid.Templates, as: Templates
  alias Fluid.Registers, as: Registers
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Blocks, as: Blocks
  alias Fluid.Tags, as: Tags

  def render(%Templates{root: root}, %Contexts{}=context) do
    { output, context } = render([], root, context)
    { :ok, Enum.join(output), context }
  end

  def render(output, [], %Contexts{}=context) do
    { output, context }
  end
  def render(output, [h|t], %Contexts{}=context) do
    { output, context } = render(output, h, context)
    case context do
      %Contexts{extended: false, break: false, continue: false} -> render(output, t, context)
      _ -> render(output, [], context)
    end
  end

  def render(output, <<text::binary>>, %Contexts{}=context) do
    { output ++ [text], context }
  end

  def render(output, %Variables{}=v, %Contexts{}=context) do
    { rendered, context } = Variables.lookup(v, context)
    { output ++ [rendered], context }
  end

  def render(output, %Tags{name: name}=tag, %Contexts{}=context) do
    { mod, Tags } = Registers.lookup(name)
    mod.render(output, tag, context)
  end

  def render(output, %Blocks{name: name}=block, %Contexts{}=context) do
    case Registers.lookup(name) do
      { mod, Blocks } -> mod.render(output, block, context)
      nil -> render(output, block.nodelist, context)
    end
  end

end

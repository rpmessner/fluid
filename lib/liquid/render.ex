defmodule Liquid.Render do
  @moduledoc """
  Cleans and prepares assign and context and calls the render function
  inside tags/blocks
  """
  alias Liquid.{Block, Context, Registers, Tag, Template, Variable}

  @spec render(template :: %Template{}, context :: %Context{}) :: {:ok, String.t(), %Context{}}
  def render(%Template{root: root}, %Context{} = context) do
    {output, context} = render([], root, context)
    {:ok, to_text(output), context}
  end

  def render(output, [], %Context{} = context) do
    {output, context}
  end

  def render(output, [h | t], %Context{} = context) do
    {output, context} = render(output, h, context)

    case context do
      %Context{extended: false, break: false, continue: false} -> render(output, t, context)
      _ -> render(output, [], context)
    end
  end

  def render(output, text, %Context{} = context) when is_binary(text) do
    {[text | output], context}
  end

  def render(output, %Variable{} = variable, %Context{} = context) do
    rendered = variable |> Variable.lookup(context) |> join_list()
    {[rendered | output], context}
  end

  def render(output, %Tag{name: name} = tag, %Context{} = context) do
    {mod, Tag} = Registers.lookup(name)
    mod.render(output, tag, context)
  end

  def render(output, %Block{name: name} = block, %Context{} = context) do
    case Registers.lookup(name) do
      {mod, Block} -> mod.render(output, block, context)
      nil -> render(output, block.nodelist, context)
    end
  end

  def to_text(list), do: list |> List.flatten() |> Enum.reverse() |> Enum.join()

  defp join_list(input) when is_list(input), do: input |> List.flatten() |> Enum.join()

  defp join_list(input), do: input
end

defmodule Liquid.Capture do
  alias Liquid.Block
  alias Liquid.Context
  alias Liquid.Template

  def parse(%Block{}=block, %Template{}=template) do
    {block, template }
  end

  def render(output, %Block{markup: markup, nodelist: content}, %Context{}=context) do
    to_atom = Regex.run(Liquid.variable_parser, markup) |> hd |> String.to_atom
    {block_output, context } = Liquid.Render.render(output, content, context)
    result_assign = context.assigns |> Dict.put(to_atom, block_output)
    context = %{context | assigns: result_assign}
    {output, context}
  end
end

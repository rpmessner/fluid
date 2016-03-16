defmodule Liquid.Capture do
  alias Liquid.Block
  alias Liquid.Context
  alias Liquid.Variable
  alias Liquid.Template

  def parse(%Block{}=block, %Template{}=template) do
    {block, template }
  end

  def render(output, %Block{markup: markup}, %Context{}=context) do
    to_atom = markup |> String.to_atom
    variable = Variable.create(markup)
  end
end
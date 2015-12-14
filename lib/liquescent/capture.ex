defmodule Liquescent.Capture do
  alias Liquescent.Block
  alias Liquescent.Context
  alias Liquescent.Variable
  alias Liquescent.Template
  require IEx

  def parse(%Block{}=block, %Template{}=template) do
    {block, template }
  end

  def render(output, %Block{markup: markup}, %Context{}=context) do
    to_atom = markup |> String.to_atom
    variable = Variable.create(markup)
    IEx.pry
  end
end
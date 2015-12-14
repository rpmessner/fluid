defmodule Liquescent.Capture do
  alias Liquescent.Blocks
  alias Liquescent.Context
  alias Liquescent.Variables
  alias Liquescent.Templates
  require IEx

  def parse(%Blocks{}=block, %Templates{}=template) do
    {block, template }
  end

  def render(output, %Blocks{markup: markup}, %Context{}=context) do
    to_atom = markup |> String.to_atom
    variable = Variables.create(markup)
    IEx.pry
  end
end
defmodule Liquescent.Capture do
  alias Liquescent.Blocks
  alias Liquescent.Contexts
  alias Liquescent.Variables
  alias Liquescent.Templates
  require IEx

  def parse(%Blocks{}=block, %Templates{}=template) do
    {block, template }
  end

  def render(output, %Blocks{markup: markup}, %Contexts{}=context) do
    to_atom = markup |> String.to_atom
    variable = Variables.create(markup)
    IEx.pry
  end
end
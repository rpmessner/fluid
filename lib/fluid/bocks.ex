defmodule Fluid.Blocks do
  def create(markup) do
    [name|rest] = String.split(markup, " ")
    name = binary_to_atom(name, :utf8)
    Fluid.Block[name: name, markup: Enum.join(rest, " ")]
  end
end
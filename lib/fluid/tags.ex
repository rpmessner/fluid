defmodule Fluid.Tags do
  def create(markup) do
    [name|rest] = String.split(markup, " ")
    Fluid.Tag[name: name |> binary_to_atom(:utf8), markup: Enum.join(rest, " ")]
  end
end

defmodule Fluid.Tags do
  defstruct name: nil, markup: nil, parts: [], attributes: []

require IEx
  def create(markup) do
    [name|rest] = String.split(markup, " ")
    %Fluid.Tags{name: name |> String.to_atom, markup: Enum.join(rest, " ")}
  end
end

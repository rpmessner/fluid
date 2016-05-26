defmodule Liquid.Tag do
  defstruct name: nil, markup: nil, parts: [], attributes: [], blank: false

  def create(markup) do
    [name|rest] = String.split(markup, " ")
    %Liquid.Tag{name: name |> String.to_atom, markup: Enum.join(rest, " ")}
  end
end

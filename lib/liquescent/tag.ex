defmodule Liquescent.Tag do
  defstruct name: nil, markup: nil, parts: [], attributes: []

  def create(markup) do
    [name|rest] = String.split(markup, " ")
    %Liquescent.Tag{name: name |> String.to_atom, markup: Enum.join(rest, " ")}
  end
end

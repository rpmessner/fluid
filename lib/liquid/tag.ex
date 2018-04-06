defmodule Liquid.Tag do
  defstruct name: nil, markup: nil, parts: [], attributes: [], blank: false

  def create(markup) do
    destructure [name, rest], String.split(markup, " ", parts: 2)
    %Liquid.Tag{name: String.to_atom(name), markup: rest}
  end
end

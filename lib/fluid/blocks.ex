defmodule Fluid.Blocks do
  alias Fluid.Tag, as: Tag
  alias Fluid.Block, as: Block

  def create(markup) do
    [name|rest] = String.split(markup, " ")
    name = binary_to_atom(name, :utf8)
    Block[name: name, markup: Enum.join(rest, " ")]
  end

  def split(Block[nodelist: nodelist], namelist//[:else]) do
    Enum.split_while(nodelist, fn(x) ->
      !(is_record(x, Tag) and Enum.member?(namelist, x.name))
    end)
  end
end

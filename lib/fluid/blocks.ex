defmodule Fluid.Blocks do
  alias Fluid.Tag, as: Tag
  alias Fluid.Block, as: Block

  def create(markup) do
    [name|rest] = String.split(markup, " ")
    name = binary_to_atom(name, :utf8)
    Block[name: name, markup: Enum.join(rest, " ")]
  end

  def split(nodes), do: split(nodes, [:else])
  def split(Block[nodelist: nodelist], namelist), do: split(nodelist, namelist)
  def split(nodelist, namelist) when is_list(nodelist) do
    Enum.split_while(nodelist, fn(x) ->
      !(is_record(x, Tag) and Enum.member?(namelist, x.name))
    end)
  end
end

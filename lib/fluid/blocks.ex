defmodule Fluid.Blocks do
  defstruct name: nil, markup: nil, condition: nil, parts: [], iterator: [], nodelist: [], elselist: []

  alias Fluid.Tags, as: Tags
  alias Fluid.Blocks, as: Blocks

  def create(markup) do
    [name|rest] = String.split(markup, " ")
    name = String.to_atom(name)

    %Blocks{name: name, markup: Enum.join(rest, " ")}
  end

  def split(nodes), do: split(nodes, [:else])
  def split(%Blocks{nodelist: nodelist}, namelist), do: split(nodelist, namelist)
  def split(nodelist, namelist) when is_list(nodelist) do
    Enum.split_while(nodelist, fn(x) ->

      !(is_map(x) and x.__struct__ == Tags and Enum.member?(namelist, x.name))
    end)
  end
end

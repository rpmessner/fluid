defmodule Fluid.ForElse do
  alias Fluid.Render, as: Render

  defrecord ForLoop, [:name, :length, :index, :index0, :rindex, :rindex0, :first, :last]

  def syntax, do: %r/(\w+)\s+in\s+(#{Fluid.quoted_fragment}+)\s*(reversed)?/

  def parse(Fluid.Block[]=block, presets), do: { block, presets }

  def render(output, Fluid.Block[nodelist: nodelist, markup: markup], context) do
    # [item|collection] Regex.scan(syntax, markup)
    # collection = Variable.lookup
    # iterator = Iterator
    # { output, _ } =
    { output, context }
  end

  # defp each(output, ForLoop[]=it, nodelist, context) do
  #   Render.render(output, nodelist, context)
  # end
end

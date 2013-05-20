defmodule Fluid.ForElse do
  alias Fluid.Render, as: Render

  defrecord ForLoop, [:name, :length, :index, :index0, :rindex, :rindex0, :first, :last]

  def syntax, do: %r/(\w+)\s+in\s+(#{Fluid.quoted_fragment}+)\s*(reversed)?/

  def parse(Fluid.Block[]=block, presets), do: { block, presets }

  def render(output, Fluid.Block[nodelist: nodelist, markup: markup], assigns, presets) do
    # [item|collection] Regex.scan(syntax, markup)
    # collection = Variable.lookup
    # iterator = Iterator
    # { output, _ } =
    { output, assigns }
  end
  # defp each(output, Iteration[rindex0: 0]=it, nodelist, assigns, presets), do: { ,  }

  #   Render.render(output, nodelist, assigns, presets)
  # end
end

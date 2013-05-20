defmodule Fluid.ForElse do
  alias Fluid.Render, as: Render
  alias Fluid.Block, as: Block
  alias Fluid.Blocks, as: Blocks
  alias Fluid.Variables, as: Variables
  alias Fluid.Context, as: Context

  defrecord Iterator, collection: nil, item: nil, reversed: false, attributes: [], forloop: []

  def syntax, do: %r/(\w+)\s+in\s+(#{Fluid.quoted_fragment}+)\s*(reversed)?/

  def parse(Block[]=block, presets) do
    block = parse_iterator(block)
    case Blocks.split(block) do
      { true_block, [_,false_block] } ->
        { block.nodelist(true_block).elselist(false_block), presets }
      { _, [] } -> { block, presets }
    end
  end

  defp parse_iterator(Block[markup: markup]=block) do
    [[item|[collection|reversed]]] = Regex.scan(syntax, markup)
    collection = Variables.create(collection)
    reversed   = !(reversed |> Enum.first |> nil?)
    attributes = Fluid.tag_attributes |> Regex.scan(markup)
    item       = item |> binary_to_atom(:utf8)
    block.iterator(Iterator[item: item, collection: collection,
                            attributes: attributes, reversed: reversed])
  end

  def render(output, Block[iterator: it]=block, Context[]=context) do
    { list, _ } = Variables.lookup(it.collection, context)
    list = if it.reversed, do: Enum.reverse(list), else: list
    each(output, list, block, context)
  end

  defp each(output, [], Block[], Context[]=context), do: { output, context }
  defp each(output, [h|t]=list, Block[nodelist: nodelist, iterator: it]=block, Context[assigns: assigns]=context) do
    forloop = next_forloop(it, list |> Enum.count)
    block   = block.iterator(forloop |> it.forloop)
    assigns = assigns |> Dict.put(:forloop, forloop)
    assigns = assigns |> Dict.put(it.item, h)
    { output, _ } = Render.render(output, nodelist, assigns |> context.assigns)
    each(output, t, block, context)
  end

  defp next_forloop(Iterator[forloop: []]=it, count) do
    [index:   1,
     index0:  0,
     rindex:  count,
     rindex0: count - 1,
     length:  count,
     first:   true,
     last:    count == 1]
  end

  defp next_forloop(Iterator[forloop: loop]=it, count) do
    [index:   loop[:index] + 1,
     index0:  loop[:index0] + 1,
     rindex:  loop[:rindex] - 1,
     rindex0: loop[:rindex0] - 1,
     length:  loop[:length],
     first:   false,
     last:    count == 1]
  end

end

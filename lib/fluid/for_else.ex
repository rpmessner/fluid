defmodule Fluid.ForElse do
  alias Fluid.Render, as: Render
  alias Fluid.Blocks, as: Blocks
  alias Fluid.Blocks, as: Blocks
  alias Fluid.Variables, as: Variables
  alias Fluid.Contexts, as: Contexts
  defmodule Iterator do
    defstruct collection: nil, item: nil, reversed: false,
                        limit: nil, offset: nil, forloop: []
  end

require IEx
  def syntax, do: ~r/(\w+)\s+in\s+(#{Fluid.quoted_fragment}+)\s*(reversed)?/

  def parse(%Blocks{}=block, %Fluid.Templates{}=t) do
    block = %{block | iterator: parse_iterator(block) }
    case Blocks.split(block) do
      { true_block, [_,false_block] } ->
        { block.nodelist(true_block).elselist(false_block), t }
      { _, [] } -> { block, t }
    end
  end

  defp parse_iterator(%Blocks{markup: markup}) do
    [[_,item|[collection|reversed]]] = Regex.scan(syntax, markup)
    collection = Variables.create(collection)
    IEx.pry
    reversed   = !(reversed |> List.first |> is_nil)
    attributes = Fluid.tag_attributes |> Regex.scan(markup)
    limit      = attributes |> parse_attribute("limit") |> Variables.create
    offset     = attributes |> parse_attribute("offset", "0") |> Variables.create
    item       = item |> String.to_atom
    Iterator[item: item, collection: collection,
             limit: limit, offset: offset, reversed: reversed]
  end

  defp parse_attribute(attributes, name, default \\ "nil") do
    attributes |> Enum.reduce(default, fn(x, ret) ->
      case x do
        [_, ^name, <<attribute::binary>>] -> attribute
        [_|_] -> ret
      end
    end)
  end

  def render(output, %Blocks{iterator: it}=block, %Contexts{}=context) do
    { list, _ } = Variables.lookup(it.collection, context)
    cond do
      is_list(list) and Enum.count(list) > 0 ->
        list = if it.reversed, do: Enum.reverse(list), else: list
        each(output, list, block, context)
      true -> Render.render(output, block.elselist, context)
    end
  end

  defp each(output, [], %Blocks{}=block, %Contexts{}=context), do: { output, remember_limit(block, context) }
  defp each(output, [h|t]=list, %Blocks{iterator: it}=block, %Contexts{assigns: assigns}=context) do
    forloop = next_forloop(it, list |> Enum.count)
    block   = block.iterator(forloop |> it.forloop)
    assigns = assigns |> Dict.put(:forloop, forloop) |> Dict.put(it.item, h)
    { output, block_context } = if should_render?(block, forloop, context) do
      Render.render(output, block.nodelist, assigns |> context.assigns)
      else { output, context }
    end
    case block_context do
      %Contexts{break: true} -> each(output, [], block, context)
      _ -> each(output, t, block, context)
    end
  end

  defp remember_limit(%Blocks{iterator: it}, context) do
    { limit, context } = lookup_limit(it, context)
    limit      = limit || 0
    key        = it.collection.name |> String.to_atom
    remembered = context.offsets[key] || 0
    context.offsets |> Dict.put(key, remembered + limit) |> context.offsets
  end

  defp should_render?(%Blocks{iterator: %Iterator{}=it}, forloop, context) do
    { limit, _ }  = lookup_limit(it, context)
    { offset, _ } = lookup_offset(it, context)
    cond do
      forloop[:index] <= offset        -> false
      limit |> is_nil                    -> true
      forloop[:index] > limit + offset -> false
      true                             -> true
    end
  end

  defp lookup_limit(%Iterator{limit: limit}, %Contexts{}=context) do
    Variables.lookup(limit, context)
  end

  defp lookup_offset(%Iterator{offset: offset}=it, %Contexts{}=context) do
    case offset.name do
      "continue" ->
        offset = context.offsets[it.collection.name |> String.to_atom]
        { offset || 0, context }
      <<_::binary>> -> Variables.lookup(offset, context)
    end
  end

  defp next_forloop(%Iterator{forloop: []}, count) do
    [index:   1,
     index0:  0,
     rindex:  count,
     rindex0: count - 1,
     length:  count,
     first:   true,
     last:    count == 1]
  end

  defp next_forloop(%Iterator{forloop: loop}, count) do
    [index:   loop[:index]  + 1,
     index0:  loop[:index0] + 1,
     rindex:  loop[:rindex]  - 1,
     rindex0: loop[:rindex0] - 1,
     length:  loop[:length],
     first:   false,
     last:    count == 1]
  end

end

defmodule Fluid.Break do
  alias Fluid.Tags, as: Tags
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Templates, as: Templates

  def parse(%Tags{}=tag, %Templates{}=template), do: { tag, template }

  def render(output, %Tags{}, %Contexts{}=context) do
    { output, context.break(true) }
  end
end

defmodule Fluid.Continue do
  alias Fluid.Tags, as: Tags
  alias Fluid.Contexts, as: Contexts

  def parse(%Tags{}=tag, template), do: { tag, template }

  def render(output, %Tags{}, %Contexts{}=context) do
    { output, context.continue(true) }
  end
end

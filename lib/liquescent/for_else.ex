defmodule Liquescent.ForElse do
  alias Liquescent.Render
  alias Liquescent.Blocks
  alias Liquescent.Blocks
  alias Liquescent.Variable
  alias Liquescent.Context
  defmodule Iterator do
    defstruct collection: nil, item: nil, reversed: false,
                        limit: nil, offset: nil, forloop: []
  end

  def syntax, do: ~r/(\w+)\s+in\s+(#{Liquescent.quoted_fragment}+)\s*(reversed)?/

  def parse(%Blocks{}=block, %Liquescent.Template{}=t) do
    block = %{block | iterator: parse_iterator(block) }
    case Blocks.split(block) do
      { true_block, [_,false_block] } ->
        { %{block | nodelist: true_block, elselist: false_block}, t }
      { _, [] } -> { block, t }
    end
  end

  defp parse_iterator(%Blocks{markup: markup}) do
    [[_,item|[collection|reversed]]] = Regex.scan(syntax, markup)
    collection = Variable.create(collection)
    reversed   = !(reversed |> List.first |> is_nil)
    attributes = Liquescent.tag_attributes |> Regex.scan(markup)
    limit      = attributes |> parse_attribute("limit") |> Variable.create
    offset     = attributes |> parse_attribute("offset", "0") |> Variable.create
    item       = item |> String.to_atom
    %Iterator{item: item, collection: collection,
             limit: limit, offset: offset, reversed: reversed}
  end

  defp parse_attribute(attributes, name, default \\ "nil") do
    attributes |> Enum.reduce(default, fn(x, ret) ->
      case x do
        [_, ^name, <<attribute::binary>>] -> attribute
        [_|_] -> ret
      end
    end)
  end

  def render(output, %Blocks{iterator: it}=block, %Context{}=context) do
    { list, _ } = Variable.lookup(it.collection, context)
    cond do
      is_list(list) and Enum.count(list) > 0 ->
        list = if it.reversed, do: Enum.reverse(list), else: list
        each(output, list, block, context)
      true -> Render.render(output, block.elselist, context)
    end
  end

  defp each(output, [], %Blocks{}=block, %Context{}=context), do: { output, remember_limit(block, context) }
  defp each(output, [h|t]=list, %Blocks{iterator: it}=block, %Context{assigns: assigns}=context) do
    forloop = next_forloop(it, list |> Enum.count)
    block   = %{ block | iterator: %{it | forloop: forloop }}
    assigns = assigns |> Dict.put(:forloop, forloop) |> Dict.put(it.item, h)
    { output, block_context } = if should_render?(block, forloop, context) do
      Render.render(output, block.nodelist, %{context | assigns: assigns})
      else { output, context }
    end
    case block_context do
      %Context{break: true} -> each(output, [], block, context)
      _ -> each(output, t, block, context)
    end
  end

  defp remember_limit(%Blocks{iterator: it}, context) do
    { limit, context } = lookup_limit(it, context)
    limit      = limit || 0
    key        = it.collection.name |> String.to_atom
    remembered = context.offsets[key] || 0
    %{ context | offsets: context.offsets |> Dict.put(key, remembered + limit) }
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

  defp lookup_limit(%Iterator{limit: limit}, %Context{}=context) do
    Variable.lookup(limit, context)
  end

  defp lookup_offset(%Iterator{offset: offset}=it, %Context{}=context) do
    case offset.name do
      "continue" ->
        offset = context.offsets[it.collection.name |> String.to_atom]
        { offset || 0, context }
      <<_::binary>> -> Variable.lookup(offset, context)
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

defmodule Liquescent.Break do
  alias Liquescent.Tags, as: Tags
  alias Liquescent.Context, as: Context
  alias Liquescent.Template, as: Template

  def parse(%Tags{}=tag, %Template{}=template), do: { tag, template }

  def render(output, %Tags{}, %Context{}=context) do
    { output, %{context | break: true } }
  end
end

defmodule Liquescent.Continue do
  alias Liquescent.Tags, as: Tags
  alias Liquescent.Context, as: Context

  def parse(%Tags{}=tag, template), do: { tag, template }

  def render(output, %Tags{}, %Context{}=context) do
    { output, %{context | continue: true } }
  end
end

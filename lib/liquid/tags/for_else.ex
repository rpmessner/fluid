defmodule Liquid.ForElse do
  alias Liquid.Render
  alias Liquid.Block
  alias Liquid.Variable
  alias Liquid.Context
  alias Liquid.Expression
  alias Liquid.RangeLookup
  defmodule Iterator do
    defstruct name: nil, collection: nil, item: nil, reversed: false,
                        limit: nil, offset: nil, forloop: %{}
  end

  def syntax, do: ~r/(\w+)\s+in\s+(#{Liquid.quoted_fragment}+)\s*(reversed)?/

  def parse(%Block{nodelist: nodelist}=block, %Liquid.Template{}=t) do
    block = %{block | iterator: parse_iterator(block) }
    case Block.split(block) do
      { true_block, [_,false_block] } ->
        is_blank = Blank.blank?([true_block|false_block])
        { %{block | nodelist: true_block, elselist: false_block, blank: is_blank}, t }
      { _, [] } ->
          is_blank = Blank.blank?(nodelist)
        { %{block | blank: is_blank}, t }
    end
  end

  defp parse_iterator(%Block{markup: markup}) do
    [[_,item|[orig_collection|reversed]]] = Regex.scan(syntax, markup)
    collection = Expression.parse(orig_collection)
    reversed   = !(reversed |> List.first |> is_nil)
    attributes = Liquid.tag_attributes |> Regex.scan(markup)
    limit      = attributes |> parse_attribute("limit") |> Variable.create
    offset     = attributes |> parse_attribute("offset", "0") |> Variable.create

    %Iterator{name: orig_collection, item: item, collection: collection,
             limit: limit, offset: offset, reversed: reversed}
  end

  defp parse_attribute(attributes, name, default \\ "nil") do
    attributes |> Enum.reduce(default, fn(x, ret) ->
      case x do
        [_, ^name, attribute] when is_binary(attribute) -> attribute
        _ -> ret
      end
    end)
  end

  def render(output, %Block{iterator: it}=block, %Context{}=context) do
    list = parse_collection(it.collection, context)
    if is_list(list) and Enum.count(list) > 0 do
      list = if it.reversed, do: Enum.reverse(list), else: list
      each(output, make_ref(), list, block, context)
    else
      Render.render(output, block.elselist, context)
    end
  end

  defp parse_collection(list, _context) when is_list(list), do: list
  defp parse_collection(%Variable{} = variable, context) do
    Variable.lookup(variable, context) |> elem(0)
  end

  defp parse_collection(%RangeLookup{} = range, context) do
    RangeLookup.parse(range, context)
  end

  def each(output, _, [], %Block{}=block, %Context{}=context), do: { output, remember_limit(block, context) }
  def each(output, prev, [h|t]=list, %Block{iterator: it}=block, %Context{assigns: assigns}=context) do
    forloop = next_forloop(it, list)
    block   = %{ block | iterator: %{it | forloop: forloop }}
    assigns = assigns |> Map.put("forloop", forloop)
                      |> Map.put(it.item, h)
                      |> Map.put("changed", {prev,h})
    { output, block_context } = cond do
      should_render?(block, forloop, context) ->
        if block.blank do
          { _, context } = Render.render(output, block.nodelist, %{context | assigns: assigns})
          { output, context }
        else
          Render.render(output, block.nodelist, %{context | assigns: assigns})
        end
      true -> { output, context }
    end
    if block_context.break == true, do: t = []
    each(output, h, t, block, %{context | assigns: block_context.assigns})
  end

  defp remember_limit(%Block{iterator: it}, context) do
    { limit, context } = lookup_limit(it, context)
    limit      = limit || 0
    remembered = context.offsets[it.name] || 0
    %{ context | offsets: context.offsets |> Map.put(it.name, remembered + limit) }
  end

  defp should_render?(%Block{iterator: %Iterator{}=it}, forloop, context) do
    { limit, _ }  = lookup_limit(it, context)
    { offset, _ } = lookup_offset(it, context)

    cond do
      forloop["index"] <= offset        -> false
      limit |> is_nil                    -> true
      forloop["index"] > limit + offset -> false
      true                             -> true
    end
  end

  defp lookup_limit(%Iterator{limit: limit}, %Context{}=context) do
    Variable.lookup(limit, context)
  end

  defp lookup_offset(%Iterator{offset: offset}=it, %Context{}=context) do
    case offset.name do
      "continue" ->
        offset = context.offsets[it.name]
        { offset || 0, context }
      <<_::binary>> -> Variable.lookup(offset, context)
    end
  end

  defp next_forloop(%Iterator{forloop: loop}, count) when map_size(loop) < 1 do
    count = count |> Enum.count
    %{"index" => 1,
     "index0" => 0,
     "rindex" => count,
     "rindex0"=> count - 1,
     "length" => count,
     "first"  => true,
     "last"   => count == 1}
  end

  defp next_forloop(%Iterator{forloop: loop}, _count) do
    %{"index" => loop["index"]  + 1,
     "index0" => loop["index0"] + 1,
     "rindex" => loop["rindex"]  - 1,
     "rindex0"=> loop["rindex0"] - 1,
     "length" => loop["length"],
     "first"  => false,
     "last"   => loop["rindex0"] == 1}
  end

end

defmodule Liquid.Break do
  alias Liquid.Tag, as: Tag
  alias Liquid.Context, as: Context
  alias Liquid.Template, as: Template

  def parse(%Tag{}=tag, %Template{}=template), do: { tag, template }

  def render(output, %Tag{}, %Context{}=context) do
    { output, %{context | break: true } }
  end
end

defmodule Liquid.Continue do
  alias Liquid.Tag, as: Tag
  alias Liquid.Context, as: Context

  def parse(%Tag{}=tag, template), do: { tag, template }

  def render(output, %Tag{}, %Context{}=context) do
    { output, %{context | continue: true } }
  end
end

defmodule Liquid.IfChanged do
  alias Liquid.{Template, Block}

  def parse(%Block{}=block, %Template{}=t), do: { block, t }
  def render(output, %Block{nodelist: nodelist}, context) do
    case context.assigns["changed"] do
      {l,r} when l != r -> Liquid.Render.render( output, nodelist, context)
      _ -> {output, context}
    end
  end

end

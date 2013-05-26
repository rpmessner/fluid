defmodule Fluid.Parse do
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
  alias Fluid.Registers, as: Registers
  alias Fluid.Filters, as: Filters

  def tokenize(<<string::binary>>) do
    toks = Regex.split(Fluid.tokenizer, string)
    Enum.filter(toks, fn(x) -> x != "" end)
  end

  def parse(<<string::binary>>, presets) do
    tokens = tokenize(string)
    { root, presets } = parse(Fluid.Block[name: :document], tokens, [], presets)
    Fluid.Template[root: root, presets: presets]
  end

  defp parse_node(<<name::binary>>, rest, presets) do
    case Regex.captures(Fluid.parser, name) do
      [tag: "", variable: <<markup::binary>>] -> { Variables.create(markup), rest, presets }
      [tag: <<markup::binary>>, variable: ""] ->
        [name|_] = String.split(markup, " ")
        case Registers.lookup(name) do
          { mod, Fluid.Block } ->
            block = Fluid.Blocks.create(markup)
            { block, rest, presets } = parse(block, rest, [], presets)
            { block, presets } = mod.parse(block, presets)
            { block, rest, presets }
          { mod, Fluid.Tag } ->
            tag = Fluid.Tags.create(markup)
            { tag, presets } = mod.parse(tag, presets)
            { tag, rest, presets }
          nil -> raise "unregistered tag: #{name}"
        end
      nil -> { name, rest, presets }
    end
  end

  def parse(Fluid.Block[name: :document], [], accum, presets) do
    { Fluid.Block[name: :document, nodelist: accum], presets }
  end

  def parse(Fluid.Block[name: name], [], _, _) do
    raise "No matching end for block {% #{atom_to_binary(name, :utf8)} %}"
  end

  def parse(Fluid.Block[name: name]=block, [h|t], accum, presets) do
    endblock = "end" <> atom_to_binary(name, :utf8)
    cond do
      Regex.match?(%r/{%\s*#{endblock}\s*%}/, h) ->
        { block.nodelist(accum), t, presets }
      Regex.match?(%r/{%\send.*?\s*$}/, h) ->
        raise "Unmatched block close: #{h}"
      true ->
        { result, rest, presets } = parse_node(h, t, presets)
        parse(block, rest, accum ++ [result], presets)
    end
  end
end

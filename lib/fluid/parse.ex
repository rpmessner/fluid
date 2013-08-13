defmodule Fluid.Parse do
  alias Fluid.Template, as: Template
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
  alias Fluid.Registers, as: Registers
  alias Fluid.Filters, as: Filters

  def tokenize(<<string::binary>>) do
    Regex.split(Fluid.tokenizer, string)
      |> List.flatten
      |> Enum.filter(&1 != "")
  end

  def parse(<<string::binary>>, Template[]=template) do
    tokens = tokenize(string)
    { root, template } = parse(Fluid.Block[name: :document], tokens, [], template)
    template.root(root)
  end

  defp parse_node(<<name::binary>>, rest, Template[]=template) do
    case Regex.captures(Fluid.parser, name) do
      [tag: "", variable: <<markup::binary>>] -> { Variables.create(markup), rest, template }
      [tag: <<markup::binary>>, variable: ""] ->
        [name|_] = String.split(markup, " ")
        case Registers.lookup(name) do
          { mod, Fluid.Block } ->
            block = Fluid.Blocks.create(markup)
            { block, rest, template } = parse(block, rest, [], template)
            { block, template } = mod.parse(block, template)
            { block, rest, template }
          { mod, Fluid.Tag } ->
            tag = Fluid.Tags.create(markup)
            { tag, template } = mod.parse(tag, template)
            { tag, rest, template }
          nil -> raise "unregistered tag: #{name}"
        end
      nil -> { name, rest, template }
    end
  end

  def parse(Fluid.Block[name: :document]=block, [], accum, Template[]=template) do
    { block.nodelist(accum), template }
  end

  def parse(Fluid.Block[name: name], [], _, _) do
    raise "No matching end for block {% #{atom_to_binary(name, :utf8)} %}"
  end

  def parse(Fluid.Block[name: name]=block, [h|t], accum, Template[]=template) do
    endblock = "end" <> atom_to_binary(name, :utf8)
    cond do
      Regex.match?(%r/{%\s*#{endblock}\s*%}/, h) ->
        { block.nodelist(accum), t, template }
      Regex.match?(%r/{%\send.*?\s*$}/, h) ->
        raise "Unmatched block close: #{h}"
      true ->
        { result, rest, template } = parse_node(h, t, template)
        parse(block, rest, accum ++ [result], template)
    end
  end
end

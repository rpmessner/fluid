defmodule Fluid.Parse do
  require IEx
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
  alias Fluid.Registers, as: Registers
  alias Fluid.Filters, as: Filters

  def tokenize(<<string::binary>>) do
    Regex.split(Fluid.tokenizer, string, on: :all_but_first, trim: true)
      |> List.flatten
      |> Enum.filter(&(&1 != ""))
  end

  def parse(<<string::binary>>, %Templates{}=template) do
    tokens = tokenize(string)
    { root, template } = parse(%Fluid.Blocks{name: :document}, tokens, [], template)
    %{ template | root: root }
  end

  defp parse_node(<<name::binary>>, rest, %Templates{}=template) do
    IEx.pry
    case Regex.named_captures(Fluid.parser, name) do
      %{"tag" => "", "variable" => <<markup::binary>>} ->
        { Variables.create(markup), rest, template }
      %{"tag" => <<markup::binary>>, "variable" => ""} ->
        [name|_] = String.split(markup, " ")
        case Registers.lookup(name) do
          { mod, Fluid.Blocks } ->
            block = Fluid.Blocks.create(markup)
            { block, rest, template } = parse(block, rest, [], template)
            { block, template } = mod.parse(block, template)
            { block, rest, template }
          { mod, Fluid.Tags } ->
            IEx.pry
            tag = Fluid.Tags.create(markup)
            { tag, template } = mod.parse(tag, template)
            { tag, rest, template }
          nil -> raise "unregistered tag: #{name}"
        end
      nil -> { name, rest, template }
    end
  end

  def parse(%Fluid.Blocks{name: :document}=block, [], accum, %Templates{}=template) do
    { %{ block | nodelist: block.nodelist ++ accum }, template }
  end

  def parse(%Fluid.Blocks{name: name}, [], _, _) do
    raise "No matching end for block {% #{to_string(name)} %}"
  end

  def parse(%Fluid.Blocks{name: name}=block, [h|t], accum, %Templates{}=template) do
    IEx.pry
    endblock = "end" <> to_string(name)
    cond do
      Regex.match?(~r/{%\s*#{endblock}\s*%}/, h) ->
        { %{ block | nodelist: block.nodelist ++ accum }, t, template }
      Regex.match?(~r/{%\send.*?\s*$}/, h) ->
        raise "Unmatched block close: #{h}"
      true ->
        { result, rest, template } = parse_node(h, t, template)
        IEx.pry
        parse(block, rest, accum ++ [result], template)
    end
  end
end

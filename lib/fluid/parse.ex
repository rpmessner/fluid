defmodule Fluid.Parse do
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
  alias Fluid.Registers, as: Registers

  def tokenize(<<string::binary>>) do
    Regex.split(Fluid.tokenizer, string, on: :all_but_first, trim: true)
      |> List.flatten
      |> Enum.filter(&(&1 != ""))
  end

  def parse(<<string::binary>>, %Templates{}=template) do
    tokens = tokenize(string)
    [name|_] = tokens
    %{ "tag" => tag_name, "variable" => _ } = Regex.named_captures(Fluid.parser, name)
    tokens = parse_tokens(string, tag_name)
    { root, template } = parse(%Fluid.Blocks{name: :document}, tokens, [], template)
    %{ template | root: root }
  end

  defp parse_tokens(<<string::binary>>, tag_name) do
    case Registers.lookup(tag_name) do
      {mod, Fluid.Blocks} ->
        try do
          mod.tokenize(string)
        rescue
          NoMethodError ->
        end
      _ ->
    end
  end

  defp parse_node(<<name::binary>>, rest, %Templates{}=template) do

    case Regex.named_captures(Fluid.parser, name) do
      %{"tag" => "", "variable" => <<markup::binary>>} ->
        { Variables.create(markup), rest, template }
      %{"tag" => <<markup::binary>>, "variable" => ""} ->
        [name|_] = String.split(markup, " ")
        case Registers.lookup(name) do
          { mod, Fluid.Blocks } ->

            block = Fluid.Blocks.create(markup)
            { block, rest, template } = try do
                mod.parse(block, rest, [], template)
              rescue
                NoMethodError -> parse(block, rest, [], template)
              end

            { block, template } = mod.parse(block, template)
            { block, rest, template }
          { mod, Fluid.Tags } ->

            tag = Fluid.Tags.create(markup)
            { tag, template } = mod.parse(tag, template)
            { tag, rest, template }
          nil -> raise "unregistered tag: #{name}"
        end
      nil -> { name, rest, template }
    end
  end

  def parse(%Fluid.Blocks{name: :document}=block, [], accum, %Templates{}=template) do
    { %{ block | nodelist: accum }, template }
  end

  def parse(%Fluid.Blocks{name: name}, [], _, _) do
    raise "No matching end for block {% #{to_string(name)} %}"
  end

  def parse(%Fluid.Blocks{name: name}=block, [h|t], accum, %Templates{}=template) do

    endblock = "end" <> to_string(name)
    cond do
      Regex.match?(~r/{%\s*#{endblock}\s*%}/, h) ->
        { %{ block | nodelist: accum }, t, template }
      Regex.match?(~r/{%\send.*?\s*$}/, h) ->
        raise "Unmatched block close: #{h}"
      true ->

        { result, rest, template } = parse_node(h, t, template)

        parse(block, rest, accum ++ [result], template)
    end
  end
end

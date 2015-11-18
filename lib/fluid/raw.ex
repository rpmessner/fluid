defmodule Fluid.Raw do
  alias Fluid.Tags
  alias Fluid.Templates
  alias Fluid.Contexts
  alias Fluid.Render

  def syntax, do: ~r/\A\s*\z/
  def full_token_possibly_invalid, do: ~r/\A(.*)#{Fluid.tag_start}\s*(\w+)\s*(.*)?#{Fluid.tag_end}\z/m

  def tokenize(<<string::binary>>) do
    Regex.split(Fluid.template_parser, string, on: :all_but_first, trim: true)
      |> List.flatten
  end

  def parse(%Fluid.Blocks{name: name}=block, [h|t], accum, %Templates{}=template) do
    if Regex.match?(Fluid.Raw.full_token_possibly_invalid, h) do

      [ extra_data, endblock | _ ] = Regex.scan(Fluid.Raw.full_token_possibly_invalid, h, capture: :all_but_first)
        |> List.flatten
      if extra_data != "" do

        block = %{ block | nodelist: accum ++ [extra_data] }
      end
      block_delimiter = "end" <> to_string(name)
      if block_delimiter == endblock do
        { block, t, template }
      else
        raise "No matching end for block {% #{to_string(name)} %}"
      end
    else

      parse(block, t, accum ++ [h], template)
    end
  end

  def parse(%Fluid.Blocks{}=block, %Fluid.Templates{}=t) do
    {block, t}
  end

  def render(output, %Fluid.Blocks{}=block, context) do
    Render.render(output, block.nodelist, context)
  end
end

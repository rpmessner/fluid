defmodule Liquid.Raw do
  alias Liquid.Tag
  alias Liquid.Template
  alias Liquid.Context
  alias Liquid.Render

  def full_token_possibly_invalid, do: ~r/\A(.*)#{Liquid.tag_start}\s*(\w+)\s*(.*)?#{Liquid.tag_end}\z/m

  def parse(%Liquid.Block{name: name}=block, [h|t], accum, %Template{}=template) do
    if Regex.match?(Liquid.Raw.full_token_possibly_invalid, h) do
      block_delimiter = "end" <> to_string(name)
      regex_result = Regex.scan(Liquid.Raw.full_token_possibly_invalid, h, capture: :all_but_first)
      [ extra_data, endblock | _ ] = regex_result |> List.flatten
      if block_delimiter == endblock do
        extra_accum = (accum ++ [extra_data])
        block = %{ block | nodelist: extra_accum |> Enum.filter(&(&1 != "")) }
        { block, t, template }
      else
        if length(t) > 0 do
          parse(block, t, accum ++ [h], template)
        else
          raise "No matching end for block {% #{to_string(name)} %}"
        end
      end
    else
      parse(block, t, accum ++ [h], template)
    end
  end

  def parse(%Liquid.Block{}=block, %Liquid.Template{}=t) do
    {block, t}
  end

  def render(output, %Liquid.Block{}=block, context) do
    Render.render(output, block.nodelist, context)
  end
end

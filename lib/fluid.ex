defrecord Fluid.Context, assigns: [], presets: []

defrecord Fluid.Template, root: nil, presets: []

defrecord Fluid.Tag, name: nil, markup: nil
defrecord Fluid.Block, name: nil, markup: nil, condition: nil,
                       iterator: [], nodelist: [], elselist: []

defrecord Fluid.Variable, name: nil, literal: nil, filters: [], parts: []
defrecord Fluid.Condition, left: nil, operator: nil, right: nil,
                           child_operator: nil, child_condition: nil

defmodule Fluid do
  def filter_arguments, do: %r/(?::|,)\s*(#{quoted_fragment})/
  def single_quote, do: "'"
  def double_quote, do: "\""
  def quote_matcher, do: %r/#{single_quote}|#{double_quote}/

  def variable_start, do: "{{"
  def variable_end, do: "}}"

  def tag_start, do: "{%"
  def tag_end, do: "%}"

  def tokenizer, do: %r/(#{tag_start}.*?#{tag_end})|(#{variable_start}.*?#{variable_end})/
  def parser, do: %r/#{tag_start}\s*(?<tag>.*?)\s*#{tag_end}|#{variable_start}\s*(?<variable>.*?)\s*#{variable_end}/g

  def quoted_string, do: "\"[^\"]*\"|'[^']*'"
  def quoted_fragment, do: "#{quoted_string}|(?:[^\s,\|'\"]|#{quoted_string})+"

  def tag_attributes, do: %r/(\w+)\s*\:\s*(#{quoted_fragment})/
  def variable_parser, do: %r/\[[^\]]+\]|[\w\-]+/
  def filter_parser, do: %r/(?:\||(?:\s*(?!(?:\|))(?:#{quoted_fragment}|\S+)\s*)+)/
end

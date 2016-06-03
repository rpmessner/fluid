defmodule Liquid do
  use Application
  import ExProf.Macro

  def start(_type, _args), do: start

  def start do
    Liquid.Registers.start
    Liquid.FileSystem.start
  end

  # Currently in for profiling
  def profile_run do
    assigns = %{"array" => Enum.to_list(1..100000)}
      markup = "{%for item in array %}{{item}}{%endfor%}"
      t = Liquid.Template.parse(markup)
      profile do
      { :ok, _rendered, _ } = Liquid.Template.render(t, assigns)
      end
  end

  def stop do
    Liquid.Registers.stop
    Liquid.FileSystem.stop
  end

  def filter_arguments, do: ~r/(?::|,)\s*(#{quoted_fragment})/
  def single_quote, do: "'"
  def double_quote, do: "\""
  def quote_matcher, do: ~r/#{single_quote}|#{double_quote}/

  def variable_start, do: "{{"
  def variable_end, do: "}}"
  def variable_incomplete_end, do: "\}\}?"

  def tag_start, do: "{%"
  def tag_end, do: "%}"

  def any_starting_tag, do: "(){{()|(){%()"

  def tokenizer, do: ~r/()#{tag_start}.*?#{tag_end}()|()#{variable_start}.*?#{variable_end}()/
  def parser, do: ~r/#{tag_start}\s*(?<tag>.*?)\s*#{tag_end}|#{variable_start}\s*(?<variable>.*?)\s*#{variable_end}/m
  def template_parser, do: ~r/#{partial_template_parser}|#{any_starting_tag}/m
  def partial_template_parser, do: "()#{tag_start}.*?#{tag_end}()|()#{variable_start}.*?#{variable_incomplete_end}()"

  def quoted_string, do: "\"[^\"]*\"|'[^']*'"
  def quoted_fragment, do: "#{quoted_string}|(?:[^\s,\|'\"]|#{quoted_string})+"

  def tag_attributes, do: ~r/(\w+)\s*\:\s*(#{quoted_fragment})/
  def variable_parser, do: ~r/\[[^\]]+\]|[\w\-]+/
  def filter_parser, do: ~r/(?:\||(?:\s*(?!(?:\|))(?:#{quoted_fragment}|\S+)\s*)+)/

  defmodule List do
    def even_elements([_,h|t]) do
      [h] ++ even_elements(t)
    end
    def even_elements([]), do: []
  end
end

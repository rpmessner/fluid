defmodule Liquid do
  @moduledoc """
  Base module for the applacation and helper  
  """
  use Application

  @doc """
  Start the application
  """
  def start(_type, _args), do: start()

  def start do
    Liquid.Filters.add_filter_modules()
    Liquid.Supervisor.start_link()
  end

  @doc """
  Stop the application
  """
  def stop, do: {:ok, "stopped"}

  @doc """
  helper that returns a regular expression patterns to filters arguments
  """
  def filter_arguments, do: ~r/(?::|,)\s*(#{quoted_fragment()})/

  @doc """
  helper that returns a sigle quote
  """
  def single_quote, do: "'"

  @doc """
  helper that returns a double quote
  """
  def double_quote, do: "\""

  @doc """
  helper that returns a regular expression patterns to match with quoted strings
  """
  def quote_matcher, do: ~r/#{single_quote()}|#{double_quote()}/

  @doc """
  helper that returns the syntax to start a variable in liquid
  """
  def variable_start, do: "{{"

  @doc """
  helper that returns the syntax to close variable in liquid 
  """
  def variable_end, do: "}}"

  @doc """
  helper that returns the syntax to  incomplete close variable in liquid 
  """
  def variable_incomplete_end, do: "\}\}?"

  @doc """
  helper that returns the syntax to open a tag in liquid 
  """
  def tag_start, do: "{%"

  @doc """
  helper that returns the syntax to close a tag in liquid 
  """
  def tag_end, do: "%}"

  @doc """
  helper that returns a regular expression patterns to match a statrting tag
  """
  def any_starting_tag, do: "(){{()|(){%()"

  @doc """
  helper that returns a regular expression patterns to match with any invalid liquid expressions
  """
  def invalid_expression,
    do: ~r/^{%.*}}$|^{{.*%}$|^{%.*([^}%]}|[^}%])$|^{{.*([^}%]}|[^}%])$|(^{{|^{%)/ms

  @doc """
  helper that returns a regular expression patterns to capture and split valid and invalid liquid expressions
  """
  def tokenizer,
    do: ~r/()#{tag_start()}.*?#{tag_end()}()|()#{variable_start()}.*?#{variable_end()}()/

  @doc """
  helper that returns a regular expression patterns to match what is a valid syntax for tags and variable
  """
  def parser,
    do:
      ~r/#{tag_start()}\s*(?<tag>.*?)\s*#{tag_end()}|#{variable_start()}\s*(?<variable>.*?)\s*#{
        variable_end()
      }/m

  @doc """
  helper that returns a regular expression patterns to match a incomplete tag viriable combination with another variable
  """
  def template_parser, do: ~r/#{partial_template_parser()}|#{any_starting_tag()}/ms

  @doc """
  helper that returns a regular expression patterns to match a  incomplete tag viriable combination
  """
  def partial_template_parser,
    do: "()#{tag_start()}.*?#{tag_end()}()|()#{variable_start()}.*?#{variable_incomplete_end()}()"

  @doc """
  helper that returns a regular expression patterns to match a quoted string
  """
  def quoted_string, do: "\"[^\"]*\"|'[^']*'"

  @doc """
  helper that returns a regular expression patterns to match a quoted fragment
  """
  def quoted_fragment, do: "#{quoted_string()}|(?:[^\s,\|'\"]|#{quoted_string()})+"

  @doc """
  helper that returns a regular expression patterns to match the syntax of liquid for tags attributes
  """
  def tag_attributes, do: ~r/(\w+)\s*\:\s*(#{quoted_fragment()})/

  @doc """
  helper that returns a regular expression patterns to match the syntax of the variable parser
  """
  def variable_parser, do: ~r/\[[^\]]+\]|[\w\-]+/

  @doc """
  helper that returns a regular expression patterns to match the syntax of the filter parser
  """
  def filter_parser, do: ~r/(?:\||(?:\s*(?!(?:\|))(?:#{quoted_fragment()}|\S+)\s*)+)/

  defmodule List do
    @moduledoc """
    A helper module to organize a list
    """
    @doc """
    Takes the elements of a list and creates a new list containing only the  elements on the even position
    """
    def even_elements([]), do: []
    def even_elements([_, h | t]), do: [h | even_elements(t)]
  end

  defmodule Atomizer do
    @moduledoc """
    Safe use of String.to_string_atom/1
    """
    def to_existing_atom(string) do
      String.to_existing_atom(string)
    rescue
      ArgumentError -> nil
    end
  end
end

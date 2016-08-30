defmodule Liquid.Filters do
  alias Liquid.HTML

  defmodule Functions do
    def size(input) when is_binary(input) do
      String.length(input)
    end

    def size(input) when is_list(input) do
      length(input)
    end

    def size(input) when is_tuple(input) do
      tuple_size(input)
    end

    def size(_), do: 0

    def downcase(input) do
      input |> to_string |> String.downcase
    end

    def upcase(input) do
      input |> to_string |> String.upcase
    end

    def capitalize(input) do
      input |> to_string |> String.capitalize
    end

    def reverse(array), do: array |> to_iterable |> Enum.reverse

    def sort(array), do: array |> Enum.sort

    def sort(array, key) do
      case array do
        [head|_] when is_map(head) -> array |> Enum.sort_by(&(&1[key]))
        _ -> array |> Enum.sort
      end
    end

    def uniq(array), do: array |> Enum.uniq

    def uniq(array, key) do
      case array do
        [head|tail] when is_map(head) -> [head|tail] |> Enum.uniq_by(&(&1[key]))
        array -> array |> Enum.uniq
      end
    end

    def join(array, separator \\ " ") do
      array |> to_iterable |> Enum.join(separator)
    end

    def truncate(input, l \\ 50)

    def truncate(nil, _), do: nil

    def truncate(input, l) do
      truncate_string = "..."
      l = l - String.length(truncate_string) - 1
      case {l, String.length(input)} do
        {l, _} when l <= 0 -> truncate_string
        {l, len} when l < len -> String.slice(input,0..l) <> truncate_string
        _ -> input
      end
    end
    
    def truncatewords(input, words \\ 15)

    def truncatewords(nil, _), do: nil

    def truncatewords(input, words) do
      truncate_string = "..."
      wordlist = input |> String.split(" ")

      l = words - 1
      case l do
        l when l < 0 -> wordlist[0]
        l when l < length(wordlist) ->
          words = wordlist |> Enum.slice(0..l) |> Enum.join(" ") 
          words <> truncate_string
        _ -> input
      end
    end


    def replace(<<string::binary>>, <<from::binary>>, <<to::binary>>) do
      string |> String.replace(from, to)
    end

    def prepend(<<string::binary>>, <<addition::binary>>) do
      addition <> string
    end

    def prepend(<<string::binary>>, nil), do: string


    def split(<<string::binary>>, <<separator::binary>>) do
      String.split(string, separator)
    end

    def split(nil, _), do: []

    def slice(list, from, to) when is_list(list) do
      list |> Enum.slice(from, to)
    end

    def slice(<<string::binary>>, from, to) do
      string |> String.slice(from, to)
    end

    def slice(list, range) when is_list(list) do
      len = length(list)
      case range do
        0 -> list
        range when range > 0 -> list |> Enum.slice(range, len)
        range -> list |> Enum.slice(len + range, len)
      end
    end

    def slice(<<string::binary>>, range) do
      len = String.length(string)
      case range do
        0 -> string
        range when range > 0 -> string |> String.slice(range, len)
        range -> string |> String.slice(len + range, len)
      end
    end


    def slice(nil, _), do: ""

    def escape(input) when is_binary(input) do
      input |> HTML.html_escape
    end

    defdelegate h(input), to: __MODULE__, as: :escape

    def escape_once(input) when is_binary(input) do
      input |> HTML.html_escape_once
    end

    def strip_html(nil), do: ""
    
    def strip_html(input) when is_binary(input) do
      input
        |> String.replace(~r/<script.*?<\/script>/m, "") 
        |> String.replace(~r/<!--.*?-->/m, "")
        |> String.replace(~r/<style.*?<\/style>/m, "")
        |> String.replace(~r/<.*?>/m, "")
    end

    def url_encode(input) when is_binary(input) do
      input |> URI.encode_www_form
    end

    def url_encode(nil), do: nil

    defp to_iterable(input) do
      case input do
        input when is_list(input) ->
          case List.first(input) do
            first when is_number(first) ->
              input |> List.flatten
            first when is_nil(first) ->
              []
            _ -> [input]
          end
          
        input when is_map(input) -> [input]
        # input when is_tuple(input) -> input 
        input -> List.wrap(input)
      end
    end

  end

  def parse(<<markup::binary>>) do
    [name|filters] = Regex.scan(Liquid.filter_parser, markup)
      |> List.flatten
      |> Enum.filter(&(&1 != "|"))
      |> Enum.map(&String.strip/1)
    filters = Enum.map(filters, fn(markup) ->
      [[_, filter]|_] = Regex.scan(~r/\s*(\w+)/, markup)
      args = Liquid.filter_arguments
        |> Regex.scan(markup)
        |> List.flatten
        |> Liquid.List.even_elements

      [String.to_atom(filter), args]
    end)
    [name|filters]
  end

  def filter([], value), do: value
  def filter([filter|rest], value) do
    [name, args] = filter
    args = Enum.map(args, fn(arg) ->
      Regex.replace(Liquid.quote_matcher, arg, "")
    end)
    ret  = apply(Functions, name, [value|args])
    filter(rest, ret)
  end

end

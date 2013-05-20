defmodule Fluid.Templates do
  use GenServer.Behaviour

  defp default_tags do
    [comment: { Fluid.Comment, Fluid.Block },
     assign:  { Fluid.Assign,  Fluid.Tag },
     elsif:   { Fluid.ElseIf,  Fluid.Tag },
     else:    { Fluid.Else,    Fluid.Tag },
     for:     { Fluid.ForElse, Fluid.Block },
     if:      { Fluid.IfElse,  Fluid.Block }]
  end

  def start do
    :gen_server.start({ :local, __MODULE__ }, __MODULE__, default_tags, [])
  end

  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  def clear do
    :gen_server.call(__MODULE__, :clear)
  end

  def handle_cast({ :register, <<name::binary>>, module, tag }, dict) do
    { :noreply, Dict.put(dict, binary_to_atom(name, :utf8), { module, tag }) }
  end

  def handle_call({ :lookup, name }, _from, dict) when is_atom(name) do
    result = Dict.get(dict, name)
    { :reply, result, dict }
  end

  def handle_call({ :lookup, <<name::binary>> }, _from, dict) do
    result = Dict.get(dict, binary_to_atom(name, :utf8))
    { :reply, result, dict }
  end

  def handle_call(:stop, _from, dict) do
    { :stop, :normal, :ok, dict }
  end

  def handle_cast(:clear, dict) do
    spawn fn ->
      Enum.each dict, fn({ _, { module, _ } }) ->
        purge_module(module)
      end
    end
    { :noreply, default_tags }
  end

  defp purge_module(module) do
    :code.delete(module)
    :code.purge(module)
  end

  def register(name, module, type) do
    :gen_server.cast(__MODULE__, { :register, name, module, type })
  end

  def lookup(name) do
    :gen_server.call(__MODULE__, { :lookup, name })
  end

  def render(Fluid.Template[]=t, assigns//[]) do
    Fluid.Render.render(t, assigns)
  end

  def parse(<<markup::binary>>, presets//[]) do
    Fluid.Parse.parse(markup, presets)
  end
end

defmodule Fluid.Render do
  alias Fluid.Variables, as: Variables
  alias Fluid.Templates, as: Templates

  def render(Fluid.Template[root: root, presets: presets], assigns) do
    { output, assigns } = render([], root, assigns, presets)
    { Enum.join(output), assigns }
  end

  def render(output, <<text::binary>>, assigns, presets) do
    { output ++ [text], assigns }
  end

  def render(output, Fluid.Variable[]=v, assigns, presets) do
    { rendered, assigns } = Variables.lookup(v, assigns, presets)
    { output ++ [rendered], assigns }
  end

  def render(output, Fluid.Tag[name: name]=tag, assigns, presets) do
    { mod, Fluid.Tag } = Templates.lookup(name)
    mod.render(output, tag, assigns, presets)
  end

  def render(output, Fluid.Block[name: name]=block, assigns, presets) do
    case Templates.lookup(name) do
      { mod, Fluid.Block } ->
        mod.render(output, block, assigns, presets)
      nil -> render(output, block.nodelist, assigns, presets)
    end
  end

  def render(output, [h|t], assigns, presets) do
    { output, assigns } = render(output, h, assigns, presets)
    render(output, t, assigns, presets)
  end

  def render(output, [], assigns, presets) do
    { output, assigns }
  end
end

defmodule Fluid.Parse do
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
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
        case Templates.lookup(name) do
          { mod, Fluid.Block } ->
            block = Fluid.Blocks.create(markup)
            { block, rest, presets } = parse(block, rest, [], presets)
            { block, presets } = mod.parse(block, presets)
            { block, rest, presets }
          { _, Fluid.Tag } -> { Fluid.Tags.create(markup), rest, presets }
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

defmodule Fluid.Tags do
  def create(markup) do
    [name|rest] = String.split(markup, " ")
    Fluid.Tag[name: name |> binary_to_atom(:utf8), markup: Enum.join(rest, " ")]
  end
end

defmodule Fluid.Blocks do
  def create(markup) do
    [name|rest] = String.split(markup, " ")
    name = binary_to_atom(name, :utf8)
    Fluid.Block[name: name, markup: Enum.join(rest, " ")]
  end
end

defmodule Fluid.Variables do
  alias Fluid.Filters, as: Filters

  defp literals, do: [nil: nil, null: nil, "": nil,
                      true: true, false: false,
                      blank: :blank?, empty: :empty?]

  def integer, do: %r/^(-?\d+)$/
  def float, do: %r/^(-?\d[\d\.]+)$/
  def quoted_string, do: %r/#{Fluid.quoted_string}/

  @doc """
    matches for [] access
  """
  def create(<<markup::binary>>) do
    [name|filters] = Filters.parse(markup)
    key = name |> String.strip |> binary_to_atom(:utf8)
    variable = Fluid.Variable[name: name, filters: filters]
    cond do
      literals      |> Dict.has_key?(key) -> literals |> Dict.get(key) |> variable.literal
      integer       |> Regex.match?(name) -> name |> binary_to_integer |> variable.literal
      float         |> Regex.match?(name) -> name |> binary_to_float |> variable.literal
      quoted_string |> Regex.match?(name) -> Fluid.quote_matcher |> Regex.replace(name, "") |> variable.literal
      true ->
        [name|_] = String.split(name, " ")
        parts = Regex.scan(Fluid.variable_parser, name)
        variable.parts(parts)
    end
  end

  def lookup(Fluid.Variable[name: name, filters: filters]=v, assigns, presets) do
    { ret, assigns } = case v do
      Fluid.Variable[literal: literal, parts: []] ->
        { literal, assigns }
      Fluid.Variable[literal: nil, parts: parts] ->
        resolve(parts, assigns, assigns, presets)
    end
    ret = Filters.filter(filters, ret)
    { ret, assigns }
  end

  defp resolve([<<?[,rest::binary>>|parts], current, assigns, presets) when is_list(assigns) do
    [index, _] = String.split(rest, "]")
    index = binary_to_integer(index)
    resolve(parts, current |> Enum.at!(index), assigns, presets)
  end

  defp resolve([<<name::binary>>|parts], current, assigns, presets) do
    { into, assigns } = resolve(name, current, assigns, presets)
    ret = resolve(parts, into, assigns, presets)
    ret
  end

  defp resolve([], current, assigns, presets) do
    { current, assigns }
  end

  defp resolve(<<name::binary>>, current, assigns, presets) when !is_list(current), do: { nil, assigns }
  defp resolve(<<name::binary>>, current, assigns, presets) when is_list(current) do
    key = binary_to_atom(name, :utf8)
    assign = Dict.get(current, key)
    preset = Dict.get(presets, key)
    cond do
      is_function(assign) ->
        assign = assign.()
        { assign, Dict.put(assigns, key, assign) }
      true -> { assign || preset, assigns }
    end
  end
end

defmodule Fluid.Filters do
  defmodule Functions do
    def split(<<string::binary>>, <<separator::binary>>) do
      String.split(string, separator)
    end

    def capitalize(<<string::binary>>) do
      String.capitalize(string)
    end
  end

  alias Fluid.Variables, as: Variables

  def parse(<<markup::binary>>) do
    filters = Regex.scan(Fluid.filter_parser, markup)
    filters = Enum.filter(filters, fn(x) -> x != "|" end)
    [name|filters] = Enum.map(filters, function(String, :strip, 1))
    filters = Enum.map(filters, fn(markup) ->
      [[filter]|rest] = Regex.scan(%r/\s*(\w+)/, markup)
      args = Fluid.filter_arguments |> Regex.scan(markup) |> List.flatten
      [binary_to_atom(filter, :utf8), args]
    end)
    [name|filters]
  end

  def filter([], value), do: value
  def filter([filter|rest], value) do
    [name, args] = filter
    args = Enum.map(args, fn(arg) ->
      Regex.replace(Fluid.quote_matcher, arg, "")
    end)
    ret  = apply(Functions, name, [value|args])
    filter(rest, ret)
  end
end

defmodule Fluid.Conditions do
  alias Fluid.Condition, as: Cond
  alias Fluid.Variables, as: Vars

  def create([h|t]=list) do
    head = create(h)
    create(head, t)
  end

  def create(condition, []), do: condition
  def create(condition, [join, right|t]) when join == "and" or join == "or" do
    right = create(right)
     join = join |> String.strip |> binary_to_atom(:utf8)
    join(join, condition, right)
  end

  def create({ left, operator, right }) do
    left = Vars.create(left)
    right = Vars.create(right)
    operator = binary_to_atom(operator, :utf8)
    Cond[left: left, operator: operator, right: right]
  end

  def create(<<left::binary>>) do
    left = Vars.create(left)
    Cond[left: left]
  end

  def join(operator, condition, { _, _, _ }=right), do: join(operator, condition, right |> create)
  def join(operator, condition, Cond[]=right) do
    right.child_condition(condition).child_operator(operator)
  end

  def evaluate(Cond[left: left, right: nil]=c, assigns//[], presets//[]) do
    { current, assigns } = Vars.lookup(left, assigns, presets)
    eval_child(!!current, c.child_operator, c.child_condition, assigns, presets)
  end

  def evaluate(Cond[left: left, right: right, operator: operator]=c, assigns//[], presets//[]) do
    { left, assigns } = Vars.lookup(left, assigns, presets)
    { right, assigns } = Vars.lookup(right, assigns, presets)
    current = eval_operator(left, operator, right)
    eval_child(!!current, c.child_operator, c.child_condition, assigns, presets)
  end

  defp eval_child(current, nil, nil, _, _), do: current

  defp eval_child(current, :and, condition, assigns, presets) do
    current and evaluate(condition, assigns, presets)
  end

  defp eval_child(current, :or, condition, assigns, presets) do
    current or evaluate(condition, assigns, presets)
  end

  defp eval_operator(left, operator, right) when (nil?(left) xor nil?(right)) and operator in [:>=, :>, :<, :<=], do: false
  defp eval_operator(left, operator, right) do
    case operator do
      :== -> left == right
      :>= -> left >= right
      :>  -> left >  right
      :<= -> left <= right
      :<  -> left <  right
      :!= -> left != right
      :<> -> left != right
      :contains -> contains(left, right)
    end
  end

  defp contains(nil, right), do: false
  defp contains(right, nil), do: false
  defp contains(<<left::binary>>, <<right::binary>>), do: contains(left |> binary_to_list, right |> binary_to_list)
  defp contains(left, <<right::binary>>) when is_list(left), do: contains(left, right |> binary_to_list)
  defp contains(<<left::binary>>, right) when is_list(right), do: contains(left |> binary_to_list, right)
  defp contains(left, right) when is_list(left) and !is_list(right), do: contains(left, [right])
  defp contains(left, right) when is_list(right) and is_list(left), do: :string.rstr(left, right) > 0
end

defmodule Fluid.ElseIf do
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Fluid.Else do
  def render(_, _, _, _), do: raise "should never get here"
end

defmodule Fluid.IfElse do
  alias Fluid.Conditions, as: Condition
  alias Fluid.Render, as: Render

  def syntax, do: %r/(#{Fluid.quoted_fragment})\s*([=!<>a-z_]+)?\s*(#{Fluid.quoted_fragment})?/
  def expressions_and_operators do
    %r/(?:\b(?:\s?and\s?|\s?or\s?)\b|(?:\s*(?!\b(?:\s?and\s?|\s?or\s?)\b)(?:#{Fluid.quoted_fragment}|\S+)\s*)+)/
  end

  def render(output, Fluid.Tag[]=tag, assigns, presets) do
    { output, assigns }
  end

  defp split_conditions(expressions) do
    expressions |> Enum.map(function(String, :strip, 1)) |> Enum.map(fn(x) ->
      case syntax |> Regex.scan(x) do
        [[left, operator, right]] -> { left, operator, right }
        [[x]] -> x
      end
    end)
  end

  defp parse_conditions(Fluid.Block[markup: markup]=block) do
    expressions = Regex.scan(expressions_and_operators, markup)
    expressions = expressions |> split_conditions |> Enum.reverse
    condition   = Condition.create(expressions)
    block       = block.condition(condition)
  end

  def parse(Fluid.Block[nodelist: nodelist]=block, presets) do
    block  = parse_conditions(block)
    blocks = Enum.split_while(nodelist, fn(x) ->
      !(is_record(x, Fluid.Tag) and x.name in [:else, :elsif])
    end)
    case blocks do
      { true_block, [Fluid.Tag[name: :elsif, markup: markup]|elsif_block] } ->
        { elseif, presets } = Fluid.Block[name: :if, markup: markup, nodelist: elsif_block] |> parse(presets)
        { block.nodelist(true_block).elselist([elseif]), presets }
      { true_block, [Fluid.Tag[name: :else]|false_block] } ->
        { block.nodelist(true_block).elselist(false_block), presets }
      { _, [] } ->
        { block, presets }
    end
  end

  def render(output, Fluid.Block[condition: condition, nodelist: nodelist, elselist: elselist]=block, assigns, presets) do
    evaled = Condition.evaluate(condition, assigns, presets)
    conditionlist = if evaled, do: nodelist, else: elselist
    Render.render(output, conditionlist, assigns, presets)
  end
end

defmodule Fluid.ForElse do
  alias Fluid.Render, as: Render

  defrecord ForLoop, [:name, :length, :index, :index0, :rindex, :rindex0, :first, :last]

  def syntax, do: %r/(\w+)\s+in\s+(#{Fluid.quoted_fragment}+)\s*(reversed)?/

  def parse(Fluid.Block[]=block, presets) do: { block, presets }

  def render(output, Fluid.Block[nodelist: nodelist, markup: markup], assigns, presets) do
    [item|collection] Regex.scan(syntax, markup)
    collection = Variable.lookup
    iterator = Iterator
    { output, _ } =
    { output, assigns }
  end
  defp each(output, Iteration[rindex0: 0]=it, nodelist, assigns, presets), do: { ,  }

    Render.render(output, nodelist, assigns, presets)
  end

  end
end

defmodule Fluid.Comment do
  def parse(Fluid.Block[]=block, presets), do: { block, presets }
  def render(output, Fluid.Block[], assigns, _), do: { output, assigns }
end

defmodule Fluid.Assign do
  alias Fluid.Variables, as: Variables

  def syntax, do: %r/([\w\-]+)\s*=\s*(.*)\s*/

  def render(output, Fluid.Tag[markup: markup], assigns, presets) do
    [[ to, from ]] = Regex.scan(syntax, markup)
    to_atom  = to |> binary_to_atom(:utf8)
    variable = Fluid.Variables.create(from)
    { from_value, assigns } = Variables.lookup(variable, assigns, presets)
    assigns = Dict.put(assigns, to_atom, from_value)
    { output, assigns }
  end
end

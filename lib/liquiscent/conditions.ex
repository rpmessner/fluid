defmodule Liquescent.Conditions do
  defstruct left: nil, operator: nil, right: nil,
              child_operator: nil, child_condition: nil

  alias Liquescent.Contexts, as: Contexts
  alias Liquescent.Variables, as: Variables
  alias Liquescent.Conditions, as: Cond
  alias Liquescent.Variables, as: Vars

  def create([h|t]) do
    head = create(h)
    create(head, t)
  end

  def create(<<left::binary>>) do
    left = Vars.create(left)
    %Cond{left: left}
  end

  def create({ <<left::binary>>, operator, <<right::binary>> }) do
    create({ left |> Vars.create, operator, right |> Vars.create})
  end

  def create({ %Variables{}=left, operator, <<right::binary>> }) do

    create({ left, operator, right |> Vars.create})
  end

  def create({ <<left::binary>>, operator, %Variables{}=right }) do
    create({ left |> Vars.create, operator, right })
  end

  def create({ %Variables{}=left, operator, %Variables{}=right }) do

    operator = String.to_atom(operator)
    %Cond{left: left, operator: operator, right: right}
  end

  def create(condition, []), do: condition
  def create(condition, [join, right|_]) when join == "and" or join == "or" do
    right = create(right)
    join  = join |> String.strip |> String.to_atom
    join(join, condition, right)
  end

  def join(operator, condition, { _, _, _ }=right), do: join(operator, condition, right |> create)
  def join(operator, condition, %Cond{}=right) do
    %{right | child_condition: condition, child_operator: operator}
  end

  def evaluate(%Cond{}=condition), do: evaluate(condition, %Contexts{})
  def evaluate(%Cond{left: left, right: nil}=condition, %Contexts{}=context) do
    { current, context } = Vars.lookup(left, context)
    eval_child(!!current, condition.child_operator, condition.child_condition, context)
  end

  def evaluate(%Cond{left: left, right: right, operator: operator}=condition, %Contexts{}=context) do
    { left, context } = Vars.lookup(left, context)
    { right, context } = Vars.lookup(right, context)
    current = eval_operator(left, operator, right)
    eval_child(!!current, condition.child_operator, condition.child_condition, context)
  end

  defp eval_child(current, nil, nil, _), do: current

  defp eval_child(current, :and, condition, context) do
    current and evaluate(condition, context)
  end

  defp eval_child(current, :or, condition, context) do
    current or evaluate(condition, context)
  end

  defp eval_operator(left, operator, right) when (is_nil(left) or is_nil(right)) and not(is_nil(left) and is_nil(right)) and operator in [:>=, :>, :<, :<=], do: false
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

  defp contains(nil, _), do: false
  defp contains(_, nil), do: false
  defp contains(<<left::binary>>, <<right::binary>>), do: contains(left |> to_char_list, right |> to_char_list)
  defp contains(left, <<right::binary>>) when is_list(left), do: contains(left, right |> to_char_list)
  defp contains(<<left::binary>>, right) when is_list(right), do: contains(left |> to_char_list, right)
  defp contains(left, right) when is_list(left) and not is_list(right), do: contains(left, [right])
  defp contains(left, right) when is_list(right) and is_list(left), do: :string.rstr(left, right) > 0
end

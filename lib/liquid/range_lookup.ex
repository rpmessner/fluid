defmodule Liquid.RangeLookup do
  defstruct range_start: 0, range_end: 0
  alias Liquid.Expression
  alias Liquid.RangeLookup
  alias Liquid.Variable
  alias Liquid.Context

  def parse(%RangeLookup{range_start: %Variable{} = range_start, range_end: %Variable{} = range_end}, %Context{} = context) do
    {left, _} = range_start |> Variable.lookup(context)
    left = left |> valid_range_value
    {right, _} = range_end |> Variable.lookup(context)
    right = right |> valid_range_value(left)

    Enum.to_list(left..right)
  end

  def parse(%RangeLookup{range_start: range_start, range_end: %Variable{} = range_end}, %Context{} = context) do
    {right, _} = range_end |> Variable.lookup(context)
    right = right |> valid_range_value(range_start)

    Enum.to_list(range_start..right)
  end

  def parse(%RangeLookup{range_start: %Variable{} = range_start, range_end: range_end}, %Context{} = context) do
    {left, _} = range_start |> Variable.lookup(context)
    left = left |> valid_range_value

    Enum.to_list(left..range_end)
  end

  def parse(left, right) do
    start_value = Expression.parse(left)
    end_value = Expression.parse(right)

    build_range(start_value, end_value)
  end

  defp valid_range_value(value, fallback) when is_binary(value) do
    if is_binary(value) do
      case Integer.parse(value) do
        :error -> fallback
        {value, _} -> value
      end
    end
  end
  defp valid_range_value(value, _), do: value
  defp valid_range_value(value), do: valid_range_value(value, 0)

  defp build_range(left, right) when is_integer(left) and is_integer(right) do
    Enum.to_list(left..right)
  end

  defp build_range(left, right) when is_map(left) or is_map(right) do
    %RangeLookup{range_start: left, range_end: right}
  end

  defp build_range(left, right) do
    left = left |> to_string |> String.to_integer
    right = right |> to_string |> String.to_integer

    Enum.to_list(left..right)
  end
end

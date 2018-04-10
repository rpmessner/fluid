defmodule Liquid.Assign do
  @moduledoc """
  Sets variables in a template
  ```
    {% assign foo = 'monkey' %}
  ```
  User can then use the variables later in the page
  ```
    {{ foo }}
  ```
  """
  alias Liquid.{Context, Tag, Variable}

  defp syntax, do: ~r/([\w\-]+)\s*=\s*(.*)\s*/

  @doc """
  Implementation of `assign` parse operations
  """
  @spec parse(%Liquid.Tag{}, %Liquid.Template{}) :: {%Liquid.Tag{}, %Liquid.Template{}}
  def parse(%Tag{} = tag, %Liquid.Template{} = template), do: {%{tag | blank: true}, template}

  @doc """
  Implementation of `assign` render operations
  """
  @spec render(%{}, %Liquid.Tag{}, %Liquid.Context{}) :: {%{}, %Liquid.Context{}}
  def render(output, %Tag{markup: markup}, %Context{} = context) do
    [[_, to, from]] = syntax() |> Regex.scan(markup)

    {from_value, context} =
      from
      |> Variable.create()
      |> Variable.lookup(context)

    result_assign = context.assigns |> Map.put(to, from_value)
    context = %{context | assigns: result_assign}
    {output, context}
  end
end

defmodule Liquid.Assign do
  @moduledoc """
  Assign sets variables in a template
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
    ```
    Liquid.Assign.parse(%Liquid.Tag{}, %Liquid.Template{})
    {%{%Liquid.Tag | blank: true}, %Liquid.Template{}}
    ```
  """
  def parse(%Tag{} = tag, %Liquid.Template{} = template), do: {%{tag | blank: true}, template}

  @doc """
  Implementation of Assign render operations
  """
  def render(output, %Tag{markup: markup}, %Context{} = context) do
    [[_, to, from]] = syntax() |> Regex.scan(markup)

    from_value =
      from
      |> Variable.create()
      |> Variable.lookup(context)

    result_assign = context.assigns |> Map.put(to, from_value)
    context = %{context | assigns: result_assign}
    {output, context}
  end
end

defmodule Liquescent.Decrement do
  alias Liquescent.Tags
  alias Liquescent.Templates
  alias Liquescent.Context
  alias Liquescent.Variables

  def parse(%Tags{}=tag, %Templates{}=template) do
    {tag, template }
  end

  def render(output, %Tags{markup: markup}, %Context{}=context) do
    to_atom = markup |> String.to_atom
    variable = Variables.create(markup)
    { value, context } = Variables.lookup(variable, context)
    value = value || 0
    result_assign = context.assigns |> Dict.put(to_atom, value - 1)
    context = %{context | assigns: result_assign }
    { output ++ [value - 1], context }
  end
end
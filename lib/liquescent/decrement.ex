defmodule Liquescent.Decrement do
  alias Liquescent.Tag
  alias Liquescent.Template
  alias Liquescent.Context
  alias Liquescent.Variable

  def parse(%Tag{}=tag, %Template{}=template) do
    {tag, template }
  end

  def render(output, %Tag{markup: markup}, %Context{}=context) do
    to_atom = markup |> String.to_atom
    variable = Variable.create(markup)
    { value, context } = Variable.lookup(variable, context)
    value = value || 0
    result_assign = context.assigns |> Dict.put(to_atom, value - 1)
    context = %{context | assigns: result_assign }
    { output ++ [value - 1], context }
  end
end
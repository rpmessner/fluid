defmodule Liquescent.Assign do
  alias Liquescent.Variables
  alias Liquescent.Tags
  alias Liquescent.Contexts

  def syntax, do: ~r/([\w\-]+)\s*=\s*(.*)\s*/

  def parse(%Tags{}=tag, %Liquescent.Templates{}=template), do: { tag, template }

  def render(output, %Tags{markup: markup}, %Contexts{}=context) do
    [[_, to, from]] = syntax |> Regex.scan(markup)
    to_atom  = to |> String.to_atom
    variable = Variables.create(from)
    { from_value, context } = Variables.lookup(variable, context)
    result_assign = context.assigns |> Dict.put(to_atom, from_value)
    context = %{context | assigns: result_assign}
    { output, context }
  end
end

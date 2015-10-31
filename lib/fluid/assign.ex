defmodule Fluid.Assign do
  alias Fluid.Variables, as: Variables
  alias Fluid.Tags, as: Tags
  alias Fluid.Contexts, as: Contexts

  def syntax, do: ~r/([\w\-]+)\s*=\s*(.*)\s*/

  def parse(%Tags{}=tag, %Fluid.Templates{}=template), do: { tag, template }

  def render(output, %Tags{markup: markup}, %Contexts{}=context) do
    [[_, to, from]] = syntax |> Regex.scan(markup)
    to_atom  = to |> String.to_atom
    variable = Variables.create(from)
    { from_value, context } = Variables.lookup(variable, context)
    context = context.assigns |> Dict.put(to_atom, from_value) |> context.assigns
    { output, context }
  end
end

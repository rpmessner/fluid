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
defmodule Fluid.Include do
  alias Fluid.Tags, as: Tags
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Templates, as: Templates
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
  alias Fluid.FileSystem, as: FileSystem
require IEx
  def syntax, do: ~r/(#{Fluid.quoted_fragment}+)(\s+(?:with|for)\s+(#{Fluid.quoted_fragment}+))?/

  def parse(%Tags{markup: markup}=tag, %Templates{}=template) do
    [parts|_]  = syntax |> Regex.scan(markup)
    tag        = parse_tag(tag, parts)
    attributes = parse_attributes(markup)
    { %{tag | attributes: attributes }, template }
  end

  defp parse_tag(%Tags{}=tag, parts) do
    case parts do
      [_, name] -> %{tag | parts: [name: name |> Variables.create]}
      [_, name," with "<>_,v] -> %{tag | parts: [name: name |> Variables.create , variable: v |> Variables.create]}
      [_, name," for "<>_,v] -> %{tag | parts: [name: name |> Variables.create, foreach: v |> Variables.create]}
    end
  end

  defp parse_attributes(markup) do
    Fluid.tag_attributes |> Regex.scan(markup) |> Enum.reduce([], fn ([_, key, val], coll) ->
      Dict.put(coll, key |> String.to_atom, val |> Variables.create)
    end)
  end

  def render(output, %Tags{parts: parts}=tag, %Contexts{}=context) do
    { file_system, root } = context |> Contexts.registers(:file_system) || FileSystem.lookup
    { name, context } = parts[:name] |> Variables.lookup(context)
    { :ok, source } = file_system.read_template_file(root, name, context)
    presets = build_presets(tag, context)
    t = Templates.parse(source, presets)
    t = %{ t | blocks: context.template.blocks |> Dict.merge(t.blocks) }
    key = name |> String.to_atom()
    cond do
      !is_nil(parts[:variable]) ->
        { item, _ } = Variables.lookup(parts[:variable], context)
        render_item(output, key, item, t, context)
      !is_nil(parts[:foreach]) ->
        { items, _ } = Variables.lookup(parts[:foreach], context)
        render_list(output, key, items, t, context)
      true -> render_item(output, key, nil, t, context)
    end
  end

  defp build_presets(%Tags{}=tag, context) do
    tag.attributes |> Enum.reduce([], fn({key, value}, coll) ->
      { value, _ } = Variables.lookup(value, context)
      Dict.put(coll, key, value)
    end)
  end

  defp render_list(output, _, [], _, context) do
    { output, context }
  end

  defp render_list(output, key, [item|rest], template, %Contexts{}=context) do
    { output, context } = render_item(output, key, item, template, context)
    render_list(output, key, rest, template, context)
  end

  defp render_item(output, _key, nil, template, %Contexts{}=context) do
    { :ok, rendered, _ } = Templates.render(template, context)
    { output ++ [rendered], context }
  end

  defp render_item(output, key, item, template, %Contexts{}=context) do
    assigns = context.assigns |> Dict.merge([{ key, item }])
    { :ok, rendered, _ } = Templates.render(template, %{context | assigns: assigns })
    { output ++ [rendered], context }
  end

end

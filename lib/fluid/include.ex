defmodule Fluid.Include do
  alias Fluid.Tag, as: Tag
  alias Fluid.Context, as: Context
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Template, as: Template
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables
  alias Fluid.FileSystem, as: FileSystem

  def syntax, do: %r/(#{Fluid.quoted_fragment}+)(\s+(?:with|for)\s+(#{Fluid.quoted_fragment}+))?/

  def parse(Tag[markup: markup]=tag, Template[]=template) do
    [parts|_]  = syntax |> Regex.scan(markup)
    tag        = parse_tag(tag, parts)
    attributes = parse_attributes(markup)
    { attributes |> tag.attributes, template }
  end

  defp parse_tag(Tag[]=tag, parts) do
    case parts do
      [name] -> tag.parts(name: name |> Variables.create)
      [name," with "<>_,v] -> tag.parts(name: name |> Variables.create, variable: v |> Variables.create)
      [name," for "<>_,v] ->
        tag.parts(name: name |> Variables.create, foreach: v |> Variables.create)
    end
  end

  defp parse_attributes(markup) do
    Fluid.tag_attributes |> Regex.scan(markup) |> Enum.reduce([], fn ([key, val], coll) ->
      Dict.put(coll, key |> binary_to_atom(:utf8), val |> Variables.create)
    end)
  end

  def render(output, Tag[parts: parts]=tag, Context[]=context) do
    { file_system, root } = context |> Contexts.registers(:file_system) || FileSystem.lookup
    { name, context } = parts[:name] |> Variables.lookup(context)
    { :ok, source } = file_system.read_template_file(root, name, context)
    presets = build_presets(tag, context)
    t = Templates.parse(source, presets)
    t = context.template.blocks |> Dict.merge(t.blocks) |> t.blocks
    key = name |> binary_to_atom(:utf8)
    cond do
      !nil?(parts[:variable]) ->
        { item, _ } = Variables.lookup(parts[:variable], context)
        render_item(output, key, item, t, context)
      !nil?(parts[:foreach]) ->
        { items, _ } = Variables.lookup(parts[:foreach], context)
        render_list(output, key, items, t, context)
      true -> render_item(output, key, nil, t, context)
    end
  end

  defp build_presets(Tag[]=tag, context) do
    tag.attributes |> Enum.reduce([], fn({key, value}, coll) ->
      { value, _ } = Variables.lookup(value, context)
      Dict.put(coll, key, value)
    end)
  end

  defp render_list(output, _, [], _, context) do
    { output, context }
  end

  defp render_list(output, key, [item|rest], template, Context[]=context) do
    { output, context } = render_item(output, key, item, template, context)
    render_list(output, key, rest, template, context)
  end

  defp render_item(output, _key, nil, template, Context[]=context) do
    { :ok, rendered, _ } = Templates.render(template, context)
    { output ++ [rendered], context }
  end

  defp render_item(output, key, item, template, Context[]=context) do
    assigns = context.assigns |> Dict.merge([{ key, item }])
    { :ok, rendered, _ } = Templates.render(template, assigns |> context.assigns)
    { output ++ [rendered], context }
  end

end

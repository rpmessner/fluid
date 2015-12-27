defmodule Liquescent.Include do
  alias Liquescent.Tag, as: Tag
  alias Liquescent.Context, as: Context
  alias Liquescent.Context, as: Context
  alias Liquescent.Template, as: Template
  alias Liquescent.Template, as: Template
  alias Liquescent.Variable, as: Variable
  alias Liquescent.FileSystem, as: FileSystem

  def syntax, do: ~r/(#{Liquescent.quoted_fragment}+)(\s+(?:with|for)\s+(#{Liquescent.quoted_fragment}+))?/

  def parse(%Tag{markup: markup}=tag, %Template{}=template) do
    [parts|_]  = syntax |> Regex.scan(markup)
    tag        = parse_tag(tag, parts)
    attributes = parse_attributes(markup)
    { %{tag | attributes: attributes }, template }
  end

  defp parse_tag(%Tag{}=tag, parts) do
    case parts do
      [_, name] -> %{tag | parts: [name: name |> Variable.create]}
      [_, name," with "<>_,v] -> %{tag | parts: [name: name |> Variable.create , variable: v |> Variable.create]}
      [_, name," for "<>_,v] -> %{tag | parts: [name: name |> Variable.create, foreach: v |> Variable.create]}
    end
  end

  defp parse_attributes(markup) do
    Liquescent.tag_attributes |> Regex.scan(markup) |> Enum.reduce([], fn ([_, key, val], coll) ->
      Dict.put(coll, key |> String.to_atom, val |> Variable.create)
    end)
  end

  def render(output, %Tag{parts: parts}=tag, %Context{}=context) do
    { file_system, root } = context |> Context.registers(:file_system) || FileSystem.lookup
    { name, context } = parts[:name] |> Variable.lookup(context)
    { :ok, source } = file_system.read_template_file(root, name, context)
    presets = build_presets(tag, context)
    t = Template.parse(source, presets)
    t = %{ t | blocks: context.template.blocks |> Dict.merge(t.blocks) }
    key = name |> String.to_atom()
    cond do
      !is_nil(parts[:variable]) ->
        { item, _ } = Variable.lookup(parts[:variable], context)
        render_item(output, key, item, t, context)
      !is_nil(parts[:foreach]) ->
        { items, _ } = Variable.lookup(parts[:foreach], context)
        render_list(output, key, items, t, context)
      true -> render_item(output, key, nil, t, context)
    end
  end

  defp build_presets(%Tag{}=tag, context) do
    tag.attributes |> Enum.reduce([], fn({key, value}, coll) ->
      { value, _ } = Variable.lookup(value, context)
      Dict.put(coll, key, value)
    end)
  end

  defp render_list(output, _, [], _, context) do
    { output, context }
  end

  defp render_list(output, key, [item|rest], template, %Context{}=context) do
    { output, context } = render_item(output, key, item, template, context)
    render_list(output, key, rest, template, context)
  end

  defp render_item(output, _key, nil, template, %Context{}=context) do
    { :ok, rendered, _ } = Template.render(template, context)
    { output ++ [rendered], context }
  end

  defp render_item(output, key, item, template, %Context{}=context) do
    assigns = context.assigns |> Dict.merge([{ key, item }])
    { :ok, rendered, _ } = Template.render(template, %{context | assigns: assigns })
    { output ++ [rendered], context }
  end

end

defmodule Liquid.Include do
  @moduledoc """
  Include allows templates to relate with other templates
  """
  alias Liquid.Tag, as: Tag
  alias Liquid.Context, as: Context
  alias Liquid.Template, as: Template
  alias Liquid.Variable, as: Variable
  alias Liquid.FileSystem, as: FileSystem

  @doc """
  Creates a regular expression pattern to match the syntax of the tag include.
  """
  def syntax,
    do: ~r/(#{Liquid.quoted_fragment()}+)(\s+(?:with|for)\s+(#{Liquid.quoted_fragment()}+))?/

  @doc """
  This function is used to parse the include tag.
  """
  @spec parse(tag :: %Liquid.Tag{}, template :: %Liquid.Template{}) ::
          {%Liquid.Tag{}, %Liquid.Template{}}
  def parse(%Tag{markup: markup} = tag, %Template{} = template) do
    [parts | _] = syntax() |> Regex.scan(markup)
    tag = parse_tag(tag, parts)
    attributes = parse_attributes(markup)
    {%{tag | attributes: attributes}, template}
  end

  defp parse_tag(%Tag{} = tag, parts) do
    case parts do
      [_, name] ->
        %{tag | parts: [name: name |> Variable.create()]}

      [_, name, " with " <> _, v] ->
        %{tag | parts: [name: name |> Variable.create(), variable: v |> Variable.create()]}

      [_, name, " for " <> _, v] ->
        %{tag | parts: [name: name |> Variable.create(), foreach: v |> Variable.create()]}
    end
  end

  defp parse_attributes(markup) do
    Liquid.tag_attributes()
    |> Regex.scan(markup)
    |> Enum.reduce(%{}, fn [_, key, val], coll ->
      Map.put(coll, key, val |> Variable.create())
    end)
  end

  @doc """
  Renders the results of the include tag.
  """
  @spec render(output :: String.t(), tag :: %Liquid.Tag{}, contex :: %Liquid.Context{}) ::
          {String.t(), %Liquid.Context{}}
  def render(output, %Tag{parts: parts} = tag, %Context{} = context) do
    {file_system, root} = context |> Context.registers(:file_system) || FileSystem.lookup()
    {name, context} = parts[:name] |> Variable.lookup(context)
    {:ok, source} = file_system.read_template_file(root, name, context)
    presets = build_presets(tag, context)
    t = Template.parse(source, presets)
    t = %{t | blocks: context.template.blocks ++ t.blocks}

    cond do
      !is_nil(parts[:variable]) ->
        {item, context} = Variable.lookup(parts[:variable], context)
        render_item(output, name, item, t, context)

      !is_nil(parts[:foreach]) ->
        {items, context} = Variable.lookup(parts[:foreach], context)
        render_list(output, name, items, t, context)

      true ->
        render_item(output, name, nil, t, context)
    end
  end

  defp build_presets(%Tag{} = tag, context) do
    tag.attributes
    |> Enum.reduce(%{}, fn {key, value}, coll ->
      {value, _} = Variable.lookup(value, context)
      Map.put(coll, key, value)
    end)
  end

  defp render_list(output, _, [], _, context) do
    {output, context}
  end

  defp render_list(output, key, [item | rest], template, %Context{} = context) do
    {output, context} = render_item(output, key, item, template, context)
    render_list(output, key, rest, template, context)
  end

  defp render_item(output, _key, nil, template, %Context{} = context) do
    {:ok, rendered, _} = Template.render(template, context)
    {[rendered] ++ output, context}
  end

  defp render_item(output, key, item, template, %Context{} = context) do
    assigns = context.assigns |> Map.merge(%{key => item})
    {:ok, rendered, _} = Template.render(template, %{context | assigns: assigns})
    {[rendered] ++ output, context}
  end
end

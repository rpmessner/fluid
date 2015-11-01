defmodule Fluid.Extends do
  alias Fluid.Tags, as: Tags
  alias Fluid.Blocks, as: Blocks
  alias Fluid.Render, as: Render
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Templates, as: Templates
  alias Fluid.Templates, as: Templates
  alias Fluid.FileSystem, as: FileSystem
require IEx
  def syntax, do: ~r/(#{Fluid.quoted_fragment}+)/

  def parse(%Tags{markup: markup}, %Templates{}=template) do
    [[_, extended]] = syntax |> Regex.scan(markup)
    { :ok, extended } = extended |> FileSystem.read_template_file
    extended = extended |> Templates.parse
    %{template | blocks: extended.blocks |> Dict.merge(template.blocks) }

    { %{extended.root | name: :extended },  template }
  end

  def render(_output, %Blocks{}=block, %Contexts{}=context) do
    { output, context } = Render.render([], block.nodelist, context)
    { output, %{context | extended: true} }
  end
end

defmodule Fluid.Inherit do
  require IEx

  alias Fluid.Blocks, as: Blocks
  alias Fluid.Render, as: Render
  alias Fluid.Contexts, as: Contexts
  alias Fluid.Templates, as: Templates
  alias Fluid.Variables, as: Variables

  def syntax, do: ~r/(#{Fluid.quoted_fragment}+)/

  def parse(%Blocks{markup: markup}=block, %Templates{}=template) do
    [[_,name]] = syntax |> Regex.scan(markup)
    name     = Fluid.quote_matcher |> Regex.replace(name, "") |> String.to_atom()
    blocks   = template.blocks |> Dict.put(name, block.nodelist)
    block    = if template.blocks[name] |> is_nil, do: block, else: block.nodelist([])
    IEx.pry
    { [name: name] ++ block.parts, blocks ++ template.blocks }
  end

  def render(output, %Blocks{nodelist: []}, context), do: { output, context }
  def render(output, %Blocks{parts: [name: name]}, %Contexts{}=context) do
    { output, context } = Render.render(output, context.blocks[name], context)
    { output, context }
  end
end

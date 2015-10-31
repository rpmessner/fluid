defmodule Fluid.Extends do
  alias Fluid.Tag, as: Tag
  alias Fluid.Block, as: Block
  alias Fluid.Render, as: Render
  alias Fluid.Context, as: Context
  alias Fluid.Template, as: Template
  alias Fluid.Templates, as: Templates
  alias Fluid.FileSystem, as: FileSystem

  def syntax, do: ~r/(#{Fluid.quoted_fragment}+)/

  def parse(%Tag{markup: markup}, %Template{}=template) do
    [[_, extended]] = syntax |> Regex.scan(markup)
    { :ok, extended } = extended |> FileSystem.read_template_file
    extended = extended |> Templates.parse
    template = extended.blocks |> Dict.merge(template.blocks) |> template.blocks
    { :extended |> extended.root.name,  template }
  end

  def render(_output, %Block{}=block, %Context{}=context) do
    { output, context } = Render.render([], block.nodelist, context)
    { output, context.extended(true) }
  end
end

defmodule Fluid.Inherit do
  alias Fluid.Block, as: Block
  alias Fluid.Render, as: Render
  alias Fluid.Context, as: Context
  alias Fluid.Template, as: Template
  alias Fluid.Variables, as: Variables

  def syntax, do: ~r/(#{Fluid.quoted_fragment}+)/

  def parse(%Block{markup: markup}=block, %Template{}=template) do
    [[_,name]] = syntax |> Regex.scan(markup)
    name     = Fluid.quote_matcher |> Regex.replace(name, "") |> binary_to_atom(:utf8)
    blocks   = template.blocks |> Dict.put(name, block.nodelist)
    block    = if template.blocks[name] |> nil?, do: block, else: block.nodelist([])
    { [name: name] |> block.parts, blocks |> template.blocks }
  end

  def render(output, %Block{nodelist: []}, context), do: { output, context }
  def render(output, %Block{parts: [name: name]}, %Context{}=context) do
    { output, context } = Render.render(output, context.blocks[name], context)
    { output, context }
  end
end

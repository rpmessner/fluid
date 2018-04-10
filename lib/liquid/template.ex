defmodule Liquid.Template do
  @moduledoc """
  Main Liquid module, all further render and parse processing passes through it
  """

  defstruct root: nil, presets: %{}, blocks: [], errors: []
  alias Liquid.{Context, Parse, Render, Template}

  @doc """
  Function that renders passed template and context to string
  """
  @spec render(t :: %Liquid.Template{}, c :: %Liquid.Context{}) :: {atom, String.t(), %Context{}}
  def render(t, c \\ %{})

  def render(%Template{} = t, %Context{} = c) do
    c = %{c | blocks: t.blocks, presets: t.presets, template: t}
    Render.render(t, c)
  end

  def render(%Template{} = t, assigns), do: render(t, assigns, [])

  def render(_, _) do
    raise Liquid.SyntaxError, message: "You can use only maps/structs to hold context data"
  end

  def render(%Template{} = t, %Context{global_filter: _global_filter} = context, options) do
    registers = Keyword.get(options, :registers, %{})
    context = %{context | registers: registers}
    render(t, context)
  end

  def render(%Template{} = t, assigns, options) when is_map(assigns) do
    context = %Context{assigns: assigns}

    context =
      case {Map.has_key?(assigns, "global_filter"), Map.has_key?(assigns, :global_filter)} do
        {true, _} ->
          %{context | global_filter: Map.fetch!(assigns, "global_filter")}

        {_, true} ->
          %{context | global_filter: Map.fetch!(assigns, :global_filter)}

        _ ->
          %{
            context
            | global_filter: Application.get_env(:liquid, :global_filter),
              extra_tags: Application.get_env(:liquid, :extra_tags, %{})
          }
      end

    render(t, context, options)
  end

  @doc """
  Function to parse markup with given presets (if any)
  """
  @spec parse(String.t(), map) :: %Liquid.Template{}
  def parse(value, presets \\ %{})

  def parse(<<markup::binary>>, presets) do
    Parse.parse(markup, %Template{presets: presets})
  end

  def parse(nil, presets) do
    Parse.parse("", %Template{presets: presets})
  end
end

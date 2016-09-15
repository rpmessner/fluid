defmodule Liquid.Template do
  @moduledoc"""
  Main Liquid module, all further render and parse processing passes through it
  """

  defstruct root: nil, presets: %{}, blocks: []
  alias Liquid.{Template, Render, Context}

  @doc """
  Function that renders passed template and context to string
  """
  @file "render.ex"
  @spec render(Liquid.Template, map) :: String.t
  def render(t, c \\ %{})
  def render(%Template{}=t, %Context{}=c) do
    c = %{c | blocks: t.blocks }
    c = %{c | presets: t.presets }
    c = %{c | template: t }
    Render.render(t, c)
  end

  def render(%Template{}=t, assigns) when is_map(assigns) do
    context = %Context{template: t,         assigns: assigns,
                       presets:  t.presets, blocks: t.blocks}
    context = case {Map.has_key?(assigns,"global_filter"), Map.has_key?(assigns,:global_filter)} do
      {true,_} -> %{context|global_filter: Map.fetch!(assigns, "global_filter")}
      {_,true} -> %{context|global_filter: Map.fetch!(assigns, :global_filter)}
      _ -> context
    end
    Render.render(t, context)
  end

  def render(_, _) do
    raise Liquid.SyntaxError, message: "You can use only maps/structs to hold context data"
  end

  @doc """
  Function to parse markup with given presets (if any)
  """
  @spec render(String.t, map) :: Liquid.Template
  def parse(<<markup::binary>>, presets \\ %{}) do
    Liquid.Parse.parse(markup, %Template{presets: presets})
  end

end

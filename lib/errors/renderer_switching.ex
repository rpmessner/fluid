defmodule Liquid.RendererSwitching do
  @moduledoc """
  Is responsible for the selection of what error mode the render will handle

  There are three error modes:
  :strict Raises a SyntaxError when invalid syntax is used
  :warn Adds errors to template.errors but continues as normal
  :lax The default mode, accepts almost anything

  Error mode is loaded Application.get_env
  """

  @error_mode Application.get_env(:ecto, :error_mode, :lax)

  @doc """
  Execute a lax render
  """
  @callback lax(output :: any, element :: %Liquid.Block{} | %Liquid.Tag{}, %Liquid.Context{}) ::
              any

  @doc """
  Execute a strict render
  """
  @callback strict(output :: any, element :: %Liquid.Block{} | %Liquid.Tag{}, %Liquid.Context{}) ::
              any

  @doc """
  Select the render based on the error_mode
  """
  def render_with_selected_renderer(output, elem, context, renderer) do
    case @error_mode do
      :strict ->
        strict_render_with_error_context(output, elem, context, renderer)

      :lax ->
        renderer.lax(output, elem, context)

      :warn ->
        try do
          strict_render_with_error_context(output, elem, context, renderer)
        rescue
          _ ->
            # TODO: save errors into somewhere
            renderer.lax(output, elem, context)
        end
    end
  end

  defp strict_render_with_error_context(_output, _elem, _context, _render) do
    raise ":strict error mode it is not implemented in this version "
  end
end

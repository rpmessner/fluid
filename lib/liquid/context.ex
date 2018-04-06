defmodule Liquid.Context do
  @moduledoc """
  This module creates a structure `(Structs)` that gives the context for rendering
  """
  defstruct assigns: %{},
            offsets: %{},
            registers: %{},
            presets: %{},
            blocks: [],
            extended: false,
            continue: false,
            break: false,
            template: nil,
            global_filter: nil,
            extra_tags: %{}

  @doc """
  it takes the context `Struct` and get the selectet key assosiated value
  """
  def registers(context, key) do
    context.registers |> Map.get(key)
  end
end

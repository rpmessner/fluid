defmodule Liquescent.Context do
  defstruct assigns: [], offsets: [], registers: [], presets: [], blocks: [],
            extended: false, continue: false, break: false, template: nil

  def registers(context, key) do
    context.registers |> Dict.get(key)
  end
end

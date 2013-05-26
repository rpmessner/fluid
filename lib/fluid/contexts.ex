defmodule Fluid.Contexts do
  def registers(context, key) do
    context.registers |> Dict.get(key)
  end
end

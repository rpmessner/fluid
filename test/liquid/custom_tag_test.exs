defmodule Liquid.CustomTagTest do
  use ExUnit.Case
  alias Liquid.{Template, Tag}

  defmodule MinusOneTag do
    def parse(%Tag{} = tag, %Template{} = context) do
      {tag, context}
    end

    def render(output, tag, context) do
      number = tag.markup |> Integer.parse() |> elem(0)
      {["#{number - 1}"] ++ output, context}
    end
  end

  setup_all do
    Liquid.Registers.register("minus_one", MinusOneTag, Tag)
    Liquid.start()
    on_exit(fn -> Liquid.stop() end)
    :ok
  end

  test "custom tag from example(almost random now :)" do
    assert_template_result("123", "123{% assign qwe = 5 %}")
    assert_template_result("4", "{% minus_one 5 %}")
    assert_template_result("a1b", "a{% minus_one 2 %}b")
  end

  defp assert_template_result(expected, markup, assigns \\ %{}) do
    assert_result(expected, markup, assigns)
  end

  defp assert_result(expected, markup, assigns) do
    template = Template.parse(markup)

    with {:ok, result, _} <- Template.render(template, assigns) do
      assert result == expected
    else
      {:error, message, _} ->
        assert message == expected
    end
  end
end

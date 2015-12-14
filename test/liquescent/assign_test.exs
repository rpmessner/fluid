Code.require_file "../../test_helper.exs", __ENV__.file

defmodule Liquescent.AssignTest do
  use ExUnit.Case
  use ExUnit.Callbacks

  setup_all do
    Liquescent.start
    on_exit fn -> Liquescent.stop end
    :ok
  end

  test :assigned_variable do
    assert_result(".foo.", "{% assign foo = values %}.{{ foo[0] }}.",
                  [values: ["foo", "bar", "baz"]])

    assert_result(".bar.", "{% assign foo = values %}.{{ foo[1] }}.",
                  [values: ["foo", "bar", "baz"]])
  end

  test :assign_with_filter do
    assert_result(".bar.", "{% assign foo = values | split: ',' %}.{{ foo[1] }}.",
                  [values: "foo,bar,baz"])
  end

  defp assert_result(expected, markup, assigns) do
    template = Liquescent.Template.parse(markup)
    { :ok, result, _ } = Liquescent.Template.render(template, assigns)
    assert result == expected
  end

end

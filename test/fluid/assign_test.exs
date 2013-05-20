Code.require_file "../../test_helper.exs", __FILE__

defmodule Fluid.AssignTest do
  use ExUnit.Case

  setup_all do
    Fluid.Templates.start
    :ok
  end

  teardown_all do
    Fluid.Templates.stop
    :ok
  end

  test :assigned_variable do
    assert_template_result(".foo.",
                           "{% assign foo = values %}.{{ foo[0] }}.",
                           [values: ["foo", "bar", "baz"]])

    assert_template_result(".bar.",
                           "{% assign foo = values %}.{{ foo[1] }}.",
                           [values: ["foo", "bar", "baz"]])
  end

  test :assign_with_filter do
    assert_template_result(".bar.",
                           "{% assign foo = values | split: ',' %}.{{ foo[1] }}.",
                           [values: "foo,bar,baz"])
  end

  defp assert_template_result(expected, markup, assigns) do
    template = Fluid.Templates.parse(markup)
    { :ok, result, _ } = Fluid.Templates.render(template, assigns)
    assert result == expected
  end

end

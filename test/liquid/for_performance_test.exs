Code.require_file "../../test_helper.exs", __ENV__.file

defmodule ForPerformanceTest do
  use ExUnit.Case

  alias Liquid.Template, as: Template

  setup_all do
    Liquid.start
    :ok
  end

  test :performance_test do
    assigns = %{"array" => Enum.to_list(1..100000)}
    assert_result(Enum.to_list(1..100000) |> Enum.join, "{%for item in array %}{{item}}{%endfor%}", assigns)
  end

  defp assert_template_result(expected, markup, assigns) do
    assert_result(expected,markup,assigns)
  end

  defp assert_result(expected, markup, assigns) do
    t = Template.parse(markup)
    { :ok, rendered, _ } = Template.render(t, assigns)
    assert rendered == expected
  end
end

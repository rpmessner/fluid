defmodule Liquid.ParsingQuirksTest do
  use ExUnit.Case

  alias Liquid.{Template, SyntaxError}

  test "error on empty filter" do
    assert_syntax_error("{{|test}}")
  end

  test "meaningless parens error" do
    markup = "a == 'foo' or (b == 'bar' and c == 'baz') or false"
    assert_syntax_error("{% if #{markup} %} YES {% endif %}")
  end

  test "unexpected characters syntax error" do
    markup = "true && false"
    assert_syntax_error("{% if #{markup} %} YES {% endif %}")
  end

  test "single close brackets" do
    assert_syntax_error("TEST {{method}")
  end
  defp assert_syntax_error(markup) do
    assert_raise(SyntaxError, fn -> Template.parse(markup) end)
  end
end

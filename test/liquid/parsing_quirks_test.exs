defmodule Liquid.ParsingQuirksTest do
  use ExUnit.Case

  alias Liquid.{Template, SyntaxError}

  test "error on empty filter" do
    assert_raise(SyntaxError, fn -> Template.parse("{{|test}}") end)
  end

  test "meaningless parens error" do
    markup = "a == 'foo' or (b == 'bar' and c == 'baz') or false"
    assert_raise(SyntaxError, fn -> Template.parse("{% if #{markup} %} YES {% endif %}") end)
  end

  test "unexpected characters syntax error" do
    markup = "true && false"
    assert_raise(SyntaxError, fn -> Template.parse("{% if #{markup} %} YES {% endif %}") end)
  end
end

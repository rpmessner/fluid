defmodule Liquid.ParsingQuirksTest do
  use ExUnit.Case

  alias Liquid.{Template, SyntaxError}

  test "error on empty filter" do
    assert_raise(SyntaxError, fn -> Template.parse("{{|test}}") end)
  end
end

defmodule Liquid.BlogTest do
  use ExUnit.Case
  alias Liquid.Template

  test "parse a middle level complex template" do
    markup = File.read!("./test/templates/blog/article.liquid")
    data = "./test/templates/blog/blog.json"
      |> File.read!()
      |> Poison.decode!()

    result = markup
      |> Template.parse()
      |> Template.render(data)
      |> elem(1)

    refute String.contains?(result, "This is a comment in a liquid template")
  end
end

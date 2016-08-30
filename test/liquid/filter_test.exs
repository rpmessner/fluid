Code.require_file "../../test_helper.exs", __ENV__.file

defmodule Liquid.FilterTest do
  use ExUnit.Case
  alias Liquid.Filters
  alias Liquid.Filters.Functions

  setup_all do
    Liquid.start
    on_exit fn -> Liquid.stop end
    :ok
  end

  test :parse_input do
    [name|filters] = "'foofoo' | replace:'foo','bar'" |> Filters.parse

    assert "'foofoo'" == name
    assert [[:replace, ["'foo'", "'bar'"]]] == filters
  end

  test :filter_parsed do
    name = "'foofoo'"
    filters = [[:replace, ["'foo'", "'bar'"]]]
    assert "'barbar'" == Filters.filter( filters, name)
  end

  test :size do
    assert 3 == Functions.size([1,2,3])
    assert 0 == Functions.size([])
    assert 0 == Functions.size(nil)
  end
  
  test :downcase do
    assert "testing", Functions.downcase("Testing")
    assert "" == Functions.downcase(nil)
  end

  test :upcase do
    assert "TESTING" == Functions.upcase("Testing")
    assert "" == Functions.upcase(nil)
  end

  test :capitalize do
    assert "Testing" == Functions.capitalize("testing")
    assert "Testing 2 words" == Functions.capitalize("testing 2 wOrds")
    assert "" == Functions.capitalize(nil)
  end

  test :prepend do
    assert "Testing" == Functions.prepend("ing", "Test")
    assert "Test" == Functions.prepend("Test", nil)
  end

  test :replace do
    assert "Tes1ing" == Functions.replace("Testing", "t", "1")
    assert "Tesing" == Functions.replace("Testing", "t", "")
  end


  test :slice do
    assert "oob" == Functions.slice("foobar", 1, 3)
    assert "oobar" == Functions.slice("foobar", 1, 1000)
    assert "" == Functions.slice("foobar", 1, 0)
    assert "o" == Functions.slice("foobar", 1, 1)
    assert "bar" == Functions.slice("foobar", 3, 3)
    assert "ar" == Functions.slice("foobar", -2, 2)
    assert "ar" == Functions.slice("foobar", -2, 1000)
    assert "r" == Functions.slice("foobar", -1)
    assert "" == Functions.slice(nil, 0)
    assert "" == Functions.slice("foobar", 100, 10)
    assert "" == Functions.slice("foobar", -100, 10)
  end

  test :slice_on_arrays do
    input = "foobar" |> String.split("")
    input = input |> List.delete("")
    assert ~w{o o b} == Functions.slice(input, 1, 3)
    assert ~w{o o b a r} == Functions.slice(input, 1, 1000)
    assert ~w{} == Functions.slice(input, 1, 0)
    assert ~w{o} == Functions.slice(input, 1, 1)
    assert ~w{b a r} == Functions.slice(input, 3, 3)
    assert ~w{a r} == Functions.slice(input, -2, 2)
    assert ~w{a r} == Functions.slice(input, -2, 1000)
    assert ~w{r} == Functions.slice(input, -1)
    assert ~w{} == Functions.slice(input, 100, 10)
    assert ~w{} == Functions.slice(input, -100, 10)
  end

  test :truncate do
    assert "1234..." == Functions.truncate("1234567890", 7)
    assert "1234567890" == Functions.truncate("1234567890", 20)
    assert "..." == Functions.truncate("1234567890", 0)
    assert "1234567890" == Functions.truncate("1234567890")
    assert "测试..." == Functions.truncate("测试测试测试测试", 5)
  end


  test :split do
    assert ["12","34"] == Functions.split("12~34", "~")
    assert ["A? "," ,Z"] == Functions.split("A? ~ ~ ~ ,Z", "~ ~ ~")
    assert ["A?Z"] == Functions.split("A?Z", "~")
    # Regexp works although Liquid does not support.
    # assert ["A","Z"] == Functions.split("AxZ", ~r/x/)
    assert [] == Functions.split(nil, " ")
  end


  test :escape do
    assert "&lt;strong&gt;" == Functions.escape("<strong>")
    assert "&lt;strong&gt;" == Functions.h("<strong>")
  end

  test :escape_once do
    assert "&lt;strong&gt;Hulk&lt;/strong&gt;" == Functions.escape_once("&lt;strong&gt;Hulk</strong>")
  end

  test :url_encode do
    assert "foo%2B1%40example.com" == Functions.url_encode("foo+1@example.com")
    assert nil == Functions.url_encode(nil)
  end

  test :truncatewords do
    assert "one two three" == Functions.truncatewords("one two three", 4)
    assert "one two..." == Functions.truncatewords("one two three", 2)
    assert "one two three" == Functions.truncatewords("one two three")
    assert "Two small (13&#8221; x 5.5&#8221; x 10&#8221; high) baskets fit inside one large basket (13&#8221;..." == Functions.truncatewords("Two small (13&#8221; x 5.5&#8221; x 10&#8221; high) baskets fit inside one large basket (13&#8221; x 16&#8221; x 10.5&#8221; high) with cover.", 15)
    assert "测试测试测试测试" == Functions.truncatewords("测试测试测试测试", 5)
  end

  test :strip_html do
    assert "test" == Functions.strip_html("<div>test</div>")
    assert "test" == Functions.strip_html(~S{<div id="test">test</div>})
    assert "" == Functions.strip_html(~S{<script type="text/javascript">document.write("some stuff");</script>})
    assert "" == Functions.strip_html(~S{<style type="text/css">foo bar</style>})
    assert "test" == Functions.strip_html(~S{<div\nclass="multiline">test</div>})
    assert "test" == Functions.strip_html(~S{<!-- foo bar \n test -->test})
    assert "" == Functions.strip_html(nil)
  end

  test :join do
    assert "1 2 3 4" == Functions.join([1, 2, 3, 4])
    assert "1 - 2 - 3 - 4" == Functions.join([1, 2, 3, 4], " - ")
  end

  test :sort do
    assert [1, 2, 3, 4] == Functions.sort([4, 3, 2, 1])
    assert [%{"a" => 1}, %{"a" => 2}, %{"a" => 3}, %{"a" => 4}] == Functions.sort([%{"a" => 4}, %{"a" => 3}, %{"a" => 1}, %{"a" => 2}], "a")
    assert [%{"a" => 1, "b" => 1}, %{"a" => 3, "b" => 2}, %{"a" => 2, "b"=> 3}] == Functions.sort([%{"a" => 3, "b" => 2}, %{"a" => 1, "b" => 1}, %{"a" => 2, "b"=> 3}], "b")
    # Elixir keyword list support
    assert ["a": 1, "a": 2, "a": 3, "a": 4] == Functions.sort([{:a, 4}, {:a, 3}, {:a, 1}, {:a, 2}], "a")
  end

  test :legacy_sort_hash do
    assert Map.to_list(%{a: 1, b: 2}) == Functions.sort(a: 1, b: 2)
  end

  test :numerical_vs_lexicographical_sort do
    assert [2, 10] == Functions.sort([10, 2])
    assert [{"a", 2}, {"a", 10}] == Functions.sort([{"a", 10}, {"a", 2}], "a")
    assert ["10", "2"] == Functions.sort(["10", "2"])
    assert [{"a", "10"}, {"a", "2"}] == Functions.sort([{"a", "10"}, {"a", "2"}], "a")
  end

  test :uniq do
    assert [1, 3, 2, 4] == Functions.uniq([1, 1, 3, 2, 3, 1, 4, 3, 2, 1])
    assert [{"a", 1}, {"a", 3}, {"a", 2}] == Functions.uniq([{"a", 1}, {"a", 3}, {"a", 1}, {"a", 2}], "a")
    # testdrop = TestDrop.new
    # assert [testdrop] == Functions.uniq([testdrop, TestDrop.new], "test")
  end

  test :reverse do
    assert [4, 3, 2, 1] == Functions.reverse([1, 2, 3, 4])
  end

  test :legacy_reverse_hash do
    assert [Map.to_list(%{a: 1, b: 2})] == Functions.reverse(a: 1, b: 2)
  end

end
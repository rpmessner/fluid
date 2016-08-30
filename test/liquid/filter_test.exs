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


  test :split do
    assert ["12","34"] == Functions.split("12~34", "~")
    assert ["A? "," ,Z"] == Functions.split("A? ~ ~ ~ ,Z", "~ ~ ~")
    assert ["A?Z"] == Functions.split("A?Z", "~")
    assert [] == Functions.split(nil, " ")
  end

  test :url_encode do
    assert "foo%2B1%40example.com" == Functions.url_encode("foo+1@example.com")
    assert nil == Functions.url_encode(nil)
  end
end
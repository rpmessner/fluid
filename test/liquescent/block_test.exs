Code.require_file "../../test_helper.exs", __ENV__.file

defmodule Liquescent.BlockTest do
  use ExUnit.Case

  defmodule TestBlock do
    def parse(b, p), do: { b, p }
  end

  defmodule TestTag do
    def parse(b, p), do: { b, p }
  end

  setup_all do
    Liquescent.start
    :ok
  end

  test "blankspace" do
    template = Liquescent.Templates.parse("  ")
    assert template.root.nodelist == ["  "]
  end

  test "variable beginning" do
    template = Liquescent.Templates.parse("{{funk}}  ")
    assert 2 == Enum.count template.root.nodelist
    assert [%Liquescent.Variables{name: name}, <<string::binary>>] = template.root.nodelist
    assert name == "funk"
    assert string == "  "
  end

  test "variable end" do
    template = Liquescent.Templates.parse("  {{funk}}")
    assert 2 == Enum.count template.root.nodelist
    assert [<<_ :: binary>>, %Liquescent.Variables{name: name}] = template.root.nodelist
    assert name == "funk"
  end

  test "variable middle" do
    template = Liquescent.Templates.parse("  {{funk}}  ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Liquescent.Variables{name: name},<<_::binary>>] = template.root.nodelist
    assert name == "funk"
  end

  test "variable many embedded fragments" do
    template = Liquescent.Templates.parse("  {{funk}} {{so}} {{brother}} ")
    assert 7 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Liquescent.Variables{},
            <<_::binary>>, %Liquescent.Variables{},
            <<_::binary>>, %Liquescent.Variables{}, <<_::binary>>] = template.root.nodelist
  end

  test "with block" do
    template = Liquescent.Templates.parse("  {% comment %} {% endcomment %} ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Liquescent.Blocks{}, <<_::binary>>] = template.root.nodelist
  end

  test "registering custom tags/blocks" do
    Liquescent.Registers.register("test", TestTag, Liquescent.Tags)
    assert { TestTag, Liquescent.Tags } = Liquescent.Registers.lookup("test")
  end

  test "with custom block" do
    Liquescent.Registers.register("testblock", TestBlock, Liquescent.Blocks)
    template = Liquescent.Templates.parse( "{% testblock %}{% endtestblock %}")
    assert [%Liquescent.Blocks{name: :testblock}] = template.root.nodelist
  end

  test "with custom tag" do
    Liquescent.Registers.register("testtag", TestTag, Liquescent.Tags)
    template = Liquescent.Templates.parse( "{% testtag %}")
    assert [%Liquescent.Tags{name: :testtag}] = template.root.nodelist
  end

end

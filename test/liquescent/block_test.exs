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
    template = Liquescent.Template.parse("  ")
    assert template.root.nodelist == ["  "]
  end

  test "variable beginning" do
    template = Liquescent.Template.parse("{{funk}}  ")
    assert 2 == Enum.count template.root.nodelist
    assert [%Liquescent.Variable{name: name}, <<string::binary>>] = template.root.nodelist
    assert name == "funk"
    assert string == "  "
  end

  test "variable end" do
    template = Liquescent.Template.parse("  {{funk}}")
    assert 2 == Enum.count template.root.nodelist
    assert [<<_ :: binary>>, %Liquescent.Variable{name: name}] = template.root.nodelist
    assert name == "funk"
  end

  test "variable middle" do
    template = Liquescent.Template.parse("  {{funk}}  ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Liquescent.Variable{name: name},<<_::binary>>] = template.root.nodelist
    assert name == "funk"
  end

  test "variable many embedded fragments" do
    template = Liquescent.Template.parse("  {{funk}} {{so}} {{brother}} ")
    assert 7 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Liquescent.Variable{},
            <<_::binary>>, %Liquescent.Variable{},
            <<_::binary>>, %Liquescent.Variable{}, <<_::binary>>] = template.root.nodelist
  end

  test "with block" do
    template = Liquescent.Template.parse("  {% comment %} {% endcomment %} ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Liquescent.Blocks{}, <<_::binary>>] = template.root.nodelist
  end

  test "registering custom tags/blocks" do
    Liquescent.Registers.register("test", TestTag, Liquescent.Tags)
    assert { TestTag, Liquescent.Tags } = Liquescent.Registers.lookup("test")
  end

  test "with custom block" do
    Liquescent.Registers.register("testblock", TestBlock, Liquescent.Blocks)
    template = Liquescent.Template.parse( "{% testblock %}{% endtestblock %}")
    assert [%Liquescent.Blocks{name: :testblock}] = template.root.nodelist
  end

  test "with custom tag" do
    Liquescent.Registers.register("testtag", TestTag, Liquescent.Tags)
    template = Liquescent.Template.parse( "{% testtag %}")
    assert [%Liquescent.Tags{name: :testtag}] = template.root.nodelist
  end

end

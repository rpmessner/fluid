Code.require_file "../../test_helper.exs", __ENV__.file

defmodule Fluid.BlockTest do
  use ExUnit.Case

  defmodule TestBlock do
    def parse(b, p), do: { b, p }
  end

  defmodule TestTag do
    def parse(b, p), do: { b, p }
  end

  setup_all do
    Fluid.start
    on_exit fn -> Fluid.stop end
    :ok
  end

  test "blankspace" do
    template = Fluid.Templates.parse("  ")
    assert template.root.nodelist == ["  "]
  end

  test "variable beginning" do
    template = Fluid.Templates.parse("{{funk}}  ")
    assert 2 == Enum.count template.root.nodelist
    assert [%Fluid.Variables{name: name}, <<string::binary>>] = template.root.nodelist
    assert name == "funk"
    assert string == "  "
  end

  test "variable end" do
    template = Fluid.Templates.parse("  {{funk}}")
    assert 2 == Enum.count template.root.nodelist
    assert [<<_ :: binary>>, %Fluid.Variables{name: name}] = template.root.nodelist
    assert name == "funk"
  end

  test "variable middle" do
    template = Fluid.Templates.parse("  {{funk}}  ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Fluid.Variables{name: name},<<_::binary>>] = template.root.nodelist
    assert name == "funk"
  end

  test "variable many embedded fragments" do
    template = Fluid.Templates.parse("  {{funk}} {{so}} {{brother}} ")
    assert 7 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Fluid.Variables{},
            <<_::binary>>, %Fluid.Variables{},
            <<_::binary>>, %Fluid.Variables{}, <<_::binary>>] = template.root.nodelist
  end

  test "with block" do
    template = Fluid.Templates.parse("  {% comment %} {% endcomment %} ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, %Fluid.Blocks{}, <<_::binary>>] = template.root.nodelist
  end

  test "registering custom tags/blocks" do
    Fluid.Registers.register("test", TestTag, Fluid.Tags)
    assert { TestTag, Fluid.Tags } = Fluid.Registers.lookup("test")
  end

  test "with custom block" do
    Fluid.Registers.register("testblock", TestBlock, Fluid.Blocks)
    template = Fluid.Templates.parse( "{% testblock %}{% endtestblock %}")
    assert [%Fluid.Blocks{name: :testblock}] = template.root.nodelist
  end

  test "with custom tag" do
    Fluid.Registers.register("testtag", TestTag, Fluid.Tags)
    template = Fluid.Templates.parse( "{% testtag %}")
    assert [%Fluid.Tags{name: :testtag}] = template.root.nodelist
  end

end

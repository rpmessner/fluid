Code.require_file "../../test_helper.exs", __FILE__

defmodule Fluid.BlockTest do
  use ExUnit.Case

  defmodule TestBlock do
    def parse(b, p), do: { b, p }
  end
  defmodule TestTag do
  end

  setup_all do
    Fluid.Templates.start
    :ok
  end

  teardown_all do
    Fluid.Templates.stop
    :ok
  end

  test "blankspace" do
    template = Fluid.Templates.parse("  ")
    assert template.root.nodelist == ["  "]
  end

  test "variable beginning" do
    template = Fluid.Templates.parse("{{funk}}  ")
    assert 2 == Enum.count template.root.nodelist
    assert [Fluid.Variable[name: name], <<string::binary>>] = template.root.nodelist
    assert name == "funk"
    assert string == "  "
  end

  test "variable end" do
    template = Fluid.Templates.parse("  {{funk}}")
    assert 2 == Enum.count template.root.nodelist
    assert [<<_ :: binary>>, Fluid.Variable[name: name]] = template.root.nodelist
    assert name == "funk"
  end

  test "variable middle" do
    template = Fluid.Templates.parse("  {{funk}}  ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, Fluid.Variable[name: name],<<_::binary>>] = template.root.nodelist
    assert name == "funk"
  end

  test "variable many embedded fragments" do
    template = Fluid.Templates.parse("  {{funk}} {{so}} {{brother}} ")
    assert 7 == Enum.count template.root.nodelist
    assert [<<_::binary>>, Fluid.Variable[],
            <<_::binary>>, Fluid.Variable[],
            <<_::binary>>, Fluid.Variable[], <<_::binary>>] = template.root.nodelist
  end

  test "with block" do
    template = Fluid.Templates.parse("  {% comment %} {% endcomment %} ")
    assert 3 == Enum.count template.root.nodelist
    assert [<<_::binary>>, Fluid.Block[], <<_::binary>>] = template.root.nodelist
  end

  test "registering custom tags/blocks" do
    Fluid.Templates.register("test", TestTag, Fluid.Tag)
    assert { TestTag, Fluid.Tag } = Fluid.Templates.lookup("test")
  end

  test "with custom block" do
    Fluid.Templates.register("testblock", TestBlock, Fluid.Block)
    template = Fluid.Templates.parse( "{% testblock %}{% endtestblock %}")
    assert [Fluid.Block[name: :testblock]] = template.root.nodelist
  end

  test "with custom tag" do
    Fluid.Templates.register("testtag", TestTag, Fluid.Tag)
    template = Fluid.Templates.parse( "{% testtag %}")
    assert [Fluid.Tag[name: :testtag]] = template.root.nodelist
  end

end

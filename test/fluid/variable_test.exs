Code.require_file "../../test_helper.exs", __ENV__.file

defmodule Fluid.VariableTest do
  use ExUnit.Case

  alias Fluid.Variables, as: Var

  test :variable do
    v = Var.create("hello")
    assert "hello" == v.name
  end

  test :filters do
    v = Var.create("hello | textileze")
    assert "hello" == v.name
    assert [[:textileze,[]]] == v.filters

    v = Var.create("hello | textileze | paragraph")
    assert "hello" == v.name
    assert [[:textileze,[]], [:paragraph,[]]] == v.filters

    v = Var.create("hello | strftime: '%Y'")
    assert "hello" == v.name
    assert [[:strftime,["'%Y'"]]] == v.filters

    v = Var.create("'typo' | link_to: 'Typo', true ")
    assert "'typo'" == v.name
    assert [[:link_to,["'Typo'", "true"]]] == v.filters

    v = Var.create("'typo' | link_to: 'Typo', false")
    assert "'typo'", v.name
    assert [[:link_to,["'Typo'", "false"]]] == v.filters

    v = Var.create("'foo' | repeat: 3")
    assert "'foo'" == v.name
    assert [[:repeat,["3"]]] == v.filters

    v = Var.create("'foo' | repeat: 3, 3")
    assert "'foo'" == v.name
    assert [[:repeat,["3","3"]]] == v.filters

    v = Var.create("'foo' | repeat: 3, 3, 3")
    assert "'foo'" == v.name
    assert [[:repeat,["3","3","3"]]] == v.filters

    v = Var.create("hello | strftime: '%Y, okay?'")
    assert "hello" == v.name
    assert [[:strftime,["'%Y, okay?'"]]] == v.filters

    v = Var.create("hello | things: \"%Y, okay?\", 'the other one'!")
    assert "hello" == v.name
    assert [[:things,["\"%Y, okay?\"","'the other one'"]]] == v.filters
  end

  test :filter_with_date_parameter do
    v = Var.create("'2006-06-06' | date: \"%m/%d/%Y\"")
    assert "'2006-06-06'" == v.name
    assert [[:date,["\"%m/%d/%Y\""]]] == v.filters
  end

  test :filters_without_whitespace do
    v = Var.create("hello | textileze | paragraph")
    assert "hello" == v.name
    assert [[:textileze,[]], [:paragraph,[]]] == v.filters

    v = Var.create("hello|textileze|paragraph")
    assert "hello" == v.name
    assert [[:textileze,[]], [:paragraph,[]]] == v.filters
  end

  test :symbol do
    v = Var.create("http://disney.com/logo.gif | image: 'med'")
    assert "http://disney.com/logo.gif" == v.name
    assert [[:image,["'med'"]]] == v.filters
  end

  test :string_single_quoted do
    v = Var.create(" \"hello\" ")
    assert "\"hello\"" == v.name
  end

  test :string_double_quoted do
    v = Var.create(" 'hello' ")
    assert "'hello'" == v.name
  end

  test :integer do
    v = Var.create(" 1000 ")
    assert "1000" == v.name
  end

  test :float do
    v = Var.create(" 1000.01 ")
    assert "1000.01" == v.name
  end

  test :string_with_special_chars do
    v = Var.create(" 'hello! $!@.;\"ddasd\" ' ")
    assert "'hello! $!@.;\"ddasd\" '" == v.name
  end

  test :string_dot do
    v = Var.create(" test.test ")
    assert "test.test" == v.name
  end
end


defmodule VariableResolutionTest do
  use ExUnit.Case

  alias Fluid.Templates, as: Templates

  setup_all do
    Fluid.start
    on_exit fn -> Fluid.stop end
    :ok
  end

  test :simple_variable do
    template = Templates.parse("{{test}}")
    { :ok, rendered, _ } = Templates.render(template, test: "worked")
    assert "worked" == rendered
    { :ok, rendered, _ } = Templates.render(template, test: "worked wonderfully")
    assert "worked wonderfully" == rendered
  end

  test :simple_with_whitespaces do
    template = Templates.parse("  {{ test }}  ")
    { :ok, rendered, _ } = Templates.render(template, test: "worked")
    assert "  worked  " == rendered
    { :ok, rendered, _ } = Templates.render(template, test: "worked wonderfully")
    assert "  worked wonderfully  " == rendered
  end

  test :ignore_unknown do
    template = Templates.parse("{{ test }}")
    { :ok, rendered, _ } = Templates.render(template)
    assert "" == rendered
  end

  test :hash_scoping do
    template = Templates.parse("{{ test.test }}")
    { :ok, rendered, _ } = Templates.render(template, test: [test: "worked"])
    assert "worked" == rendered
  end

  test :preset_assigns do
    template = Templates.parse("{{ test }}", [test: "worked"])
    { :ok, rendered, _ } = Templates.render(template)
    assert "worked" == rendered
  end

  test :reuse_parsed_template do
    template = Templates.parse("{{ greeting }} {{ name }}", greeting: "Goodbye")
    assert [greeting: "Goodbye"] == template.presets
    { :ok, rendered, _ } = Templates.render(template, greeting: "Hello", name: "Tobi")
    assert "Hello Tobi" == rendered
    { :ok, rendered, _ } = Templates.render(template, greeting: "Hello", unknown: "Tobi")
    assert "Hello " == rendered
    { :ok, rendered, _ } = Templates.render(template, greeting: "Hello", name: "Brian")
    assert "Hello Brian" == rendered
    { :ok, rendered, _ } = Templates.render(template, name: "Brian")
    assert "Goodbye Brian" == rendered
  end

  test :assigns_not_polluted_from_template do
    template = Templates.parse("{{ test }}{% assign test = 'bar' %}{{ test }}", test: "baz")
    { :ok, rendered, _ } = Templates.render(template)
    assert "bazbar" == rendered
    { :ok, rendered, _ } = Templates.render(template)
    assert "bazbar" == rendered
    { :ok, rendered, _ } = Templates.render(template, test: "foo")
    assert "foobar" == rendered
    { :ok, rendered, _ } = Templates.render(template)
    assert "bazbar" == rendered
  end
end

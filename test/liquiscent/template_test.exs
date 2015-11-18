Code.require_file "../../test_helper.exs", __ENV__.file

defmodule Liquescent.TemplateTest do
  use ExUnit.Case

  alias Liquescent.Templates, as: Templates
  alias Liquescent.Parse, as: Parse

  setup_all do
    {:ok, registry } = Liquescent.start
    {:ok, registry: registry}
  end

  test :tokenize_strings do
    assert [" "] == Parse.tokenize(" ")
    assert ["hello world"] == Parse.tokenize("hello world")
  end

  test :tokenize_variables do
    assert ["{{funk}}"] == Parse.tokenize("{{funk}}")
    assert [" ", "{{funk}}", " "] == Parse.tokenize(" {{funk}} ")
    assert [" ", "{{funk}}", " ", "{{so}}", " ", "{{brother}}", " "] == Parse.tokenize(" {{funk}} {{so}} {{brother}} ")
    assert [" ", "{{  funk  }}", " "] == Parse.tokenize(" {{  funk  }} ")
  end

  test :tokenize_blocks do
    assert ["{%comment%}"] == Parse.tokenize("{%comment%}")
    assert [" ", "{%comment%}", " "] == Parse.tokenize(" {%comment%} ")
    assert [" ", "{%comment%}", " ", "{%endcomment%}", " "] == Parse.tokenize(" {%comment%} {%endcomment%} ")
    assert ["  ", "{% comment %}", " ", "{% endcomment %}", " "] == Parse.tokenize("  {% comment %} {% endcomment %} ")
  end

  test :returns_assigns_from_assign_tags do
    t = Templates.parse("{% assign foo = 'from returned assigns' %}{{ foo }}")
    { :ok, rendered, context } = Templates.render(t)
    assert "from returned assigns" == rendered

    t = Templates.parse("{{ foo }}")
    { :ok, rendered, _ } = Templates.render(t, context)
    assert "from returned assigns" == rendered
  end

  test :instance_assigns_persist_on_same_template_parsing_between_renders do
    t = Templates.parse("{{ foo }}{% assign foo = 'foo' %}{{ foo }}")
    { :ok, rendered, context } = Templates.render(t)
    assert "foo" == rendered
    { :ok, rendered, _ } = Templates.render(t, context)
    assert "foofoo" == rendered
  end

  test :custom_assigns_do_not_persist_on_same_template do
    t = Templates.parse("{{ foo }}")

    { :ok, rendered, _ } = Templates.render(t, [foo: "from custom assigns"])
    assert "from custom assigns" == rendered
    { :ok, rendered, _ } = Templates.render(t)
    assert "" == rendered
  end

  test :template_assigns_squash_assigns do
    t = Templates.parse("{% assign foo = 'from instance assigns' %}{{ foo }}")
    { :ok, rendered, _ } = Templates.render(t)
    assert "from instance assigns" == rendered
    { :ok, rendered, _ } = Templates.render(t, [foo: "from custom assigns"])
    assert "from instance assigns" == rendered
  end

  test :template_assigns_squash_preset_assigns do
    t = Templates.parse("{% assign foo = 'from instance assigns' %}{{ foo }}", [foo: "from preset assigns"])
    { :ok, rendered, _ } = Templates.render(t)
    assert "from instance assigns" == rendered
  end

end

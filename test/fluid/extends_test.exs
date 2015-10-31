Code.require_file "../../test_helper.exs", __FILE__

defmodule ExtendsTagTest do
  defmodule TestFileSystem do
    def read_template_file(_root, template_path, _context) do
      case template_path do
        "base" ->
          { :ok, "Output / {% block content %}Hello, World!{% endblock %}" }
        "base2" ->
          { :ok, "Output / {% block content %}Hello, World!{% block tagline %}(My tagline){% endblock %}{% endblock %}" }
        "deep" ->
          { :ok, "{% extends base %}{% block content %}Deep: {{block.super}}{% endblock %}" }
        "parent-template" ->
          { :ok, "Hurrah!" }
        "partial1" ->
          { :ok, "[Partial Content1]" }
        "partial2" ->
          { :ok, "[Partial Content2]" }
        "parent-with-variable" ->
          { :ok,  "Hello, {{ name }}!" }
        "parent-with-variable2" ->
          { :ok,  "Hello, {{ name }}!!" }
        "parent-with-parent" ->
          { :ok, "{% extends parent-with-variable2 %}" }
        "nested_and_deep" ->
          { :ok, "{% extends base %}{% block content %}Deep: {{block.super}} -{% block inner %}FOO{% endblock %}-{% endblock %}" }
        "parent-with-include" -> { :ok, """
    | {% include 'partial1' %}
    | {% block thing %}{% include 'partial2' %}{% endblock %}
    """
        }
        _ -> { :ok, template_path }
      end
    end
  end

  alias Fluid.Templates, as: Templates

  use ExUnit.Case

  setup_all do
    Fluid.start
    Fluid.FileSystem.register TestFileSystem
    :ok
  end

  teardown_all do
    Fluid.stop
    :ok
  end

  test "extending a path" do
    assert_result "Hurrah!", "{% extends parent-template %}"
  end

  test "include blocks within the parent template" do
    expected = """
    | [Partial Content1]
    | [Overridden Block]
    """
    markup = """
    | {% extends parent-with-include %}
    | {% block thing %}[Overridden Block]{% endblock %}
    """
    assert_result expected, markup

    expected = """
    | [Partial Content1]
    | [Partial Content2]
    """
    markup = "{% extends parent-with-include %}"
    assert_result expected, markup
  end

  test "access the context from the inherited template" do
    assert_result "Hello, Joe!",
                  "{% extends parent-with-variable %}",
                  [name: "Joe"]
  end

  test "deep nesting of inherited templates" do
    assert_result "Hello, Joe!!",
                  "{% extends parent-with-parent %}",
                  [name: "Joe"]
  end

  test "overriding blocks from an inherited template" do
    assert_result "Output / Hola, Mundo!",
      "{% extends base %}{% block content %}Hola, Mundo!{% endblock %}"
  end

  # test "overriding block call super" do
  #   assert_result "Output / Lorem ipsum: Hello, World!",
  #     "{% extends base %}{% block content %}Lorem ipsum: {{block.super}}{% endblock %}"
  # end

  # test "deep nested includes call super within overriden blocks" do
  #   assert_result "Output / Lorem ipsum: Deep: Hello, World!",
  #     "{% extends deep %}{% block content %}Lorem ipsum: {{block.super}}{% endblock %}"
  #   assert_result "Output / Deep: Hello, World! -BAR-",
  #     "{% extends nested_and_deep %}{% block content/inner %}BAR{% endblock %}"
  # end

  # test "should allow overriding blocks from an inherited template" do
  #   assert_result "Output / Hello, World!(new tagline)",
  #     "{% extends base2 %}{% block content/tagline %}(new tagline){% endblock %}"
  # end

  defp assert_result(expected, markup), do: assert_result(expected, markup, Fluid.%Context{})
  defp assert_result(expected, markup, Fluid.%Context{}=context) do
    t = Templates.parse(markup)
    { :ok, rendered, _context } = Templates.render(t, context)
    assert expected == rendered
  end

  defp assert_result(expected, markup, assigns) do
    context = Fluid.Context[assigns: assigns]
    assert_result(expected, markup, context)
  end

end

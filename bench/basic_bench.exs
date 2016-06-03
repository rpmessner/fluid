defmodule BasicBench do
  use Benchfella
  alias Liquid.Template

  @list Enum.to_list(1..1000)

  setup_all do
    Liquid.start
    {:ok, nil}
  end

  bench "Loop list" do
    assigns = %{"array" => @list}
    markup = "{%for item in array %}{{item}}{%endfor%}"
    t = Template.parse(markup)
    { :ok, rendered, _ } = Template.render(t, assigns)
  end

end

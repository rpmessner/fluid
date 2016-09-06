Code.require_file "../../test_helper.exs", __ENV__.file
defmodule Liquid.CustomFilterTest do
  use ExUnit.Case
  use Timex
  alias Liquid.Template

  defmodule MyFilter do
  	def meaning_of_life(_), do: 42
  	def not_meaning_of_life(_), do: 2
  end

  setup_all do
    Liquid.start
	my_custom_filters = MyFilter.__info__(:functions)
      |> Enum.into(%{}, fn {key,_} -> {key, MyFilter} end)
    Application.put_env(:liquid, :custom_filters, my_custom_filters, persistent: true)

    on_exit fn -> Liquid.stop end
    :ok
  end

  test :custom_filter do
  	assert_template_result "42", "{{ 'whatever' | meaning_of_life }}"
  end
  
  test :nonexistent_in_custom_chain do
  	assert_template_result "2", "{{ 'text' | upcase | not_meaning_of_life | minus_nonexistent: 1 }}"
  end

  test :filters_custom_in_chain do
    assert_template_result "41", "{{ 'text' | upcase | nonexistent | meaning_of_life | minus: 1 }}"
  end

  defp assert_template_result(expected, markup, assigns \\ %{})

  defp assert_template_result(expected, markup, assigns) do
    assert_result(expected, markup, assigns)
  end

  defp assert_result(expected, markup, assigns) do
    template = Template.parse(markup)
    with { :ok, result, _ } <- Template.render(template, assigns) do
      assert result == expected
    else
      { :error, message, _ } ->
        assert message == expected
    end
  end

end
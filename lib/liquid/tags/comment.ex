defmodule Liquid.Comment do
  @moduledoc """
   Allows you to leave un-rendered code inside a Liquid template. Any text within the opening and closing comment blocks will not be output, and any Liquid code within will not be executed.
  Input:
    ```
      Anything you put between {% comment %} and {% endcomment %} tags
      is turned into a comment.
    ```
  Output:
    ```
      Anything you put between  tags
      is turned into a comment.
    ```
  """

  @doc """
  Implementation of Comment parse operations
  """
  @spec parse(block :: %Liquid.Block{}, template :: %Liquid.Template{}) :: {%Liquid.Block{}, %Liquid.Template{}}
  def parse(%Liquid.Block{} = block, %Liquid.Template{} = template),
    do: {%{block | blank: true, strict: false}, template}

  @doc """
  Implementation of Comment render operations
  """
  @spec render(List, %Liquid.Block{}, List) :: {List, List}
  def render(output, %Liquid.Block{}, context), do: {output, context}
end

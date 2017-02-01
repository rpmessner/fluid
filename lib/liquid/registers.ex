defmodule Liquid.Registers do

  defp default_tags do
    %{
      # defaultcontent: { Liquid.Default,  Liquid.Tag },
     continue:       { Liquid.Continue, Liquid.Tag },
     comment:        { Liquid.Comment,  Liquid.Block },
     include:        { Liquid.Include,  Liquid.Tag },
     assign:         { Liquid.Assign,   Liquid.Tag },
     block:          { Liquid.Inherit,  Liquid.Block },
     break:          { Liquid.Break,    Liquid.Tag },
     elsif:          { Liquid.ElseIf,   Liquid.Tag },
     else:           { Liquid.Else,     Liquid.Tag },
     case:           { Liquid.Case,     Liquid.Block },
     when:           { Liquid.When,     Liquid.Tag },
     for:            { Liquid.ForElse,  Liquid.Block },
     tablerow:       { Liquid.TableRow, Liquid.Block },
     ifchanged:      { Liquid.IfChanged,Liquid.Block},
     if:             { Liquid.IfElse,   Liquid.Block },
     unless:         { Liquid.Unless,   Liquid.Block},
     raw:            { Liquid.Raw,      Liquid.Block},
     increment:      { Liquid.Increment, Liquid.Tag},
     decrement:      { Liquid.Decrement, Liquid.Tag},
     cycle:          { Liquid.Cycle,     Liquid.Tag},
     capture:        { Liquid.Capture, Liquid.Block}}
  end

  def clear do
    Application.put_env(:liquid, :extra_tags, default_tags())
  end

  def lookup(name) when is_binary(name) do
    name |> String.to_atom |> lookup
  end

  def lookup(name) when is_atom(name) do
    custom_tags = Application.get_env(:liquid, :extra_tags)
    case {name, default_tags()[name], custom_tags[name]} do
      {nil, _, _} -> nil
      {_, nil, nil} -> nil
      {_, nil, custom_tag} -> custom_tag
      {_, tag, _} -> tag
    end
  end

  def lookup(_), do: nil

  def register(name, module, type) do
    custom_tags = Application.get_env(:liquid, :extra_tags) || %{}
    custom_tags = %{name |> String.to_atom => {module, type}}
      |> Map.merge(custom_tags)
    Application.put_env(:liquid, :extra_tags, custom_tags)
  end

end

# Liquid [![Hex.pm](https://img.shields.io/hexpm/v/liquid.svg)](https://hex.pm/packages/liquid) [![Hex.pm](https://img.shields.io/hexpm/dt/liquid.svg)](https://hex.pm/packages/liquid) [![Build Status](https://travis-ci.org/bettyblocks/liquid-elixir.svg?branch=master)](https://travis-ci.org/bettyblocks/liquid-elixir)

It's a templating library for Elixir.
Continuation of the fluid liquid conversion.

## Usage

Add the dependency to your mix file:

``` elixir
# mix.exs
defp deps do
  […,
   {:liquid, "~> 0.2.0"}]
end
```

You can either start the application directly:

`Liquid.start`

Or start it with your application:

``` elixir
# mix.exs
def application do
  [mod: {MyApp, []},
   applications: […, :liquid]]
end
```

Compile a template from a string:

`template = Liquid.Template.parse("{% assign hello='hello' %}{{ hello }}{{world}}")`

Render the template with a keyword list representing the local variables:

`{ :ok, rendered, _ } = Liquid.Template.render(template, %{"world" => "world"})`

The tests should give a pretty good idea of the features implemented so far.

## Custom tags and filters

You can add your own filters and tags inside your project:

``` elixir
defmodule MyFilters do
  def meaning_of_life(_), do: 42
  def one(_), do: 1
end

defmodule ExampleTag do
  def parse(%Liquid.Tag{}=tag, %Liquid.Template{}=context) do
    {tag, context}
  end

  def render(_input, tag, context) do
    number = tag.markup |> Integer.parse |> elem(0)
    {["#{number - 1}"], context}
  end
end
```

and than include them in your `config.exs` file

``` elixir
config :liquid,
  extra_filters_module: [MyFilters],
  extra_tags: %{minus_one: {ExampleTag, Liquid.Tag}}
```

Another option is to set up the tag using:
`Liquid.Registers.register("minus_one", MinusOneTag, Tag)` for tag
and for filters you should use
`Liquid.Filters.add_filters(MyFilters)`


## Missing Features

Feel free to add a bug report or pull request if you feel that anything is missing.

### todo

* Add full set of filters
* Implement table_row tag
* Fix empty check on arrays

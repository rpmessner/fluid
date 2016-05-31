# Liquid [![Hex.pm](https://img.shields.io/hexpm/v/liquid.svg)](https://hex.pm/packages/liquid) [![Hex.pm](https://img.shields.io/hexpm/dt/liquid.svg)](https://hex.pm/packages/liquid) [![Build Status](https://travis-ci.org/nulian/liquid-elixir.svg?branch=master)](https://travis-ci.org/nulian/liquid-elixir)

It's a templating library for Elixir.
Continuation of the fluid liquid conversion.

## Usage

Add the dependency to your mix file:

``` elixir
# mix.exs
defp deps do
  […,
   {:liquid, "~> 0.1.0"}]
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

`{ :ok, rendered, _ } = Liquid.Template.render(template, [world: "world"])`

The tests should give a pretty good idea of the features implemented so far.

## Missing Features

Feel free to add a bug report or pull request if you feel that anything is missing.

### todo

* Add full set of filters
* Add ability to add custom filters
* Add ability to add custom tags
* Implement table_row tag
* Fix empty check on arrays

# Fluid

It's a templating library for Elixir.  This is my first real project in Elixir (or any BEAM language, for that matter); as such, I did a fairly straight-up port of the locomotive fork of the liquid templating language: [https://github.com/locomotivecms/liquid](https://github.com/locomotivecms/liquid).

## Usage
Add the dependency to your mix file

Start the application:

`Fluid.start`

Compile a template from a string:

`template = Fluid.Templates.parse("{assign hello='hello' %}{{ hello }}{{world}}")`

Render the template with a keyword list representing the local variables:

`{ :ok, rendered } = Fluid.Templates.render(template, [world: "world"])`

The tests should give a pretty good idea of the features implemented so far.

## Missing Features

Feel free to add a bug report or pull request if you feel that anything is missing.

### todo

* Add full set of filters
* Add ability to add custom filters

## Elsewhere

If this is not for you, there is also a port of the Django template engine (which is quite similar) written for the ChicagoBoss framework: [https://github.com/evanmiller/erlydtl](https://github.com/evanmiller/erlydtl)

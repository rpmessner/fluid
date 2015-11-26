# Liquescent

It's a templating library for Elixir.  
Continuation of the fluid liquid conversion. 

## Usage
Add the dependency to your mix file

Start the application:

`Liquescent.start`

Compile a template from a string:

`template = Liquescent.Templates.parse("{% assign hello='hello' %}{{ hello }}{{world}}")`

Render the template with a keyword list representing the local variables:

`{ :ok, rendered } = Liquescent.Templates.render(template, [world: "world"])`

The tests should give a pretty good idea of the features implemented so far.

## Missing Features

Feel free to add a bug report or pull request if you feel that anything is missing.

### todo

* Add full set of filters
* Add ability to add custom filters
* Add ability to add custom tags
* Implement capture
* Implement table_row tag
* Fix empty check on arrays

## Elsewhere

If this is not for you, there is also a port of the Django template engine (which is quite similar) written for the ChicagoBoss framework: [https://github.com/evanmiller/erlydtl](https://github.com/evanmiller/erlydtl)

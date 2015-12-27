# Liquid

It's a templating library for Elixir.  
Continuation of the fluid liquid conversion. 

## Usage
Add the dependency to your mix file

Start the application:

`Liquid.start`

Compile a template from a string:

`template = Liquid.Template.parse("{% assign hello='hello' %}{{ hello }}{{world}}")`

Render the template with a keyword list representing the local variables:

`{ :ok, rendered } = Liquid.Template.render(template, [world: "world"])`

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

# Code.require_file "../../test_helper.exs", __ENV__.file
#
# defmodule Fluid.TableRowTest do
#   use ExUnit.Case
#
#   test :test_table_row do
#     assert_template_result("<tr class=\"row1\">\n<td class=\"col1\"> 1 </td><td class=\"col2\"> 2 </td><td class=\"col3\"> 3 </td></tr>\n<tr class=\"row2\"><td class=\"col1\"> 4 </td><td class=\"col2\"> 5 </td><td class=\"col3\"> 6 </td></tr>\n",
#       "{% tablerow n in numbers cols:3%} {{n}} {% endtablerow %}",
#       "numbers": [1, 2, 3, 4, 5, 6])
#
#     assert_template_result("<tr class=\"row1\">\n</tr>\n",
#       "{% tablerow n in numbers cols:3%} {{n}} {% endtablerow %}",
#       "numbers": [])
#   end
#
#   test :test_table_row_with_different_cols do
#     assert_template_result("<tr class=\"row1\">\n<td class=\"col1\"> 1 </td><td class=\"col2\"> 2 </td><td class=\"col3\"> 3 </td><td class=\"col4\"> 4 </td><td class=\"col5\"> 5 </td></tr>\n<tr class=\"row2\"><td class=\"col1\"> 6 </td></tr>\n",
#       "{% tablerow n in numbers cols:5%} {{n}} {% endtablerow %}",
#       "numbers": [1, 2, 3, 4, 5, 6])
#   end
#
#   test :test_table_col_counter do
#     assert_template_result("<tr class=\"row1\">\n<td class=\"col1\">1</td><td class=\"col2\">2</td></tr>\n<tr class=\"row2\"><td class=\"col1\">1</td><td class=\"col2\">2</td></tr>\n<tr class=\"row3\"><td class=\"col1\">1</td><td class=\"col2\">2</td></tr>\n",
#       "{% tablerow n in numbers cols:2%}{{tablerowloop.col}}{% endtablerow %}",
#       "numbers": [1, 2, 3, 4, 5, 6])
#   end
#
#   test :test_quoted_fragment do
#     assert_template_result("<tr class=\"row1\">\n<td class=\"col1\"> 1 </td><td class=\"col2\"> 2 </td><td class=\"col3\"> 3 </td></tr>\n<tr class=\"row2\"><td class=\"col1\"> 4 </td><td class=\"col2\"> 5 </td><td class=\"col3\"> 6 </td></tr>\n",
#       "{% tablerow n in collections.frontpage cols:3%} {{n}} {% endtablerow %}",
#       [collections: [ frontpage: [1, 2, 3, 4, 5, 6] ]])
#     assert_template_result("<tr class=\"row1\">\n<td class=\"col1\"> 1 </td><td class=\"col2\"> 2 </td><td class=\"col3\"> 3 </td></tr>\n<tr class=\"row2\"><td class=\"col1\"> 4 </td><td class=\"col2\"> 5 </td><td class=\"col3\"> 6 </td></tr>\n",
#       "{% tablerow n in collections["frontpage"] cols:3%} {{n}} {% endtablerow %}",
#       [ collections: [ frontpage: [1, 2, 3, 4, 5, 6] ]])
#   end
#
#   test :test_enumerable_drop do
#     assert_template_result("<tr class=\"row1\">\n<td class=\"col1\"> 1 </td><td class=\"col2\"> 2 </td><td class=\"col3\"> 3 </td></tr>\n<tr class=\"row2\"><td class=\"col1\"> 4 </td><td class=\"col2\"> 5 </td><td class=\"col3\"> 6 </td></tr>\n",
#       "{% tablerow n in numbers cols:3%} {{n}} {% endtablerow %}",
#       "numbers": ArrayDrop.new([1, 2, 3, 4, 5, 6]))
#   end
#
#   test :test_offset_and_limit do
#     assert_template_result("<tr class=\"row1\">\n<td class=\"col1\"> 1 </td><td class=\"col2\"> 2 </td><td class=\"col3\"> 3 </td></tr>\n<tr class=\"row2\"><td class=\"col1\"> 4 </td><td class=\"col2\"> 5 </td><td class=\"col3\"> 6 </td></tr>\n",
#       "{% tablerow n in numbers cols:3 offset:1 limit:6%} {{n}} {% endtablerow %}",
#       "numbers": [0, 1, 2, 3, 4, 5, 6, 7])
#   end
#
#   test :test_blank_string_not_iterable do
#     assert_template_result("<tr class=\"row1\">\n</tr>\n", "{% tablerow char in characters cols:3 %}I WILL NOT BE OUTPUT{% endtablerow %}", "characters": "")
#   end
#
#   defp assert_template_result(expected, markup) do
#     assert_result(expected, markup, [])
#   end
#
#   defp assert_template_result(expected, markup, assigns) do
#     assert_result(expected,markup,assigns)
#   end
#
#   defp assert_result(expected, markup, assigns) do
#     template = Fluid.Templates.parse(markup)
#     { :ok, result, _ } = Fluid.Templates.render(template, assigns)
#     assert result == expected
#   end
# end

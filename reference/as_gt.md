# Create a gt table object

Convert an object to a gt table.

## Usage

``` r
# S3 method for class 'atest'
as_gt(
  x,
  footnote_col = "footnote",
  rowname_col = c(),
  groupname_col = c(),
  simplify = TRUE,
  row_group_as_column = TRUE,
  ...
)

as_gt(x, ...)
```

## Arguments

- x:

  the object to convert.

- footnote_col:

  the column to add footnotes to.

- rowname_col:

  column to use as row names, if desired.

- groupname_col:

  column to use as groups, if desired.

- simplify:

  a logical variable, whether or not to simplify the table first. See
  Details.

- row_group_as_column:

  a logical variable, whether or not to

- ...:

  additional parameters, passed to `gt`.

## Details

Behind the scenes, an `atest` object has a number of columns specifying
the variables used in the original formula; these include not only the
names of the variables (`.y ~ .x | .g`) but also for categorical
variables, the values of the variables (`.y_value`, `.x_value`,
`.g_value`) and any contrasts (`.y_contrast`, `.x_contrast`) for
two-sample, pairwise, or paired comparisons. There is also a `.terms`
variable for the right hand side of a an `atest` about a model.

When `simplify = TRUE` (the default), these are converted to more
readable columns, in particular, any values or contrasts are combined
with the variable names, and new variables of `response`, `variable`,
`group`, and `model` are used instead of the variables described above.

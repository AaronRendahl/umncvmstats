# Compact a gt table

Compact a gt table. Uses `tab_options` to set `table.font.size` and the
various padding options (see Details).

## Usage

``` r
tab_compact(x, font.size = 13, padding = 1)

tab_padding(x, padding)
```

## Arguments

- x:

  the table to compact.

- font.size:

  the desired font size.

- padding:

  spacing between rows, in pixels.

## Details

Padding options that are set are `data_row.padding`,
`summary_row.padding`, `grand_summary_row.padding`, `footnotes.padding`,
`source_notes.padding`, and `row_group.padding`; all are set to the same
value.

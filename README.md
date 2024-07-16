
<!-- README.md is generated from README.Rmd. Please edit that file -->

# umncvmstats

<!-- badges: start -->
<!-- badges: end -->

The goal of umncvmstats is to …

## Installation

You can install the development version of umncvmstats from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AaronRendahl/umncvmstats")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(umncvmstats)
mtcars <- datasets::mtcars |> 
  dplyr::mutate(am=forcats::fct_recode(factor(am), 
                    "automatic"="0", "manual"="1"))

combine_tests(
  one_t_test(wt ~ factor(am), mtcars),
  two_t_test(wt ~ factor(am), mtcars))
```

<div id="rhiiucptef" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rhiiucptef table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rhiiucptef thead, #rhiiucptef tbody, #rhiiucptef tfoot, #rhiiucptef tr, #rhiiucptef td, #rhiiucptef th {
  border-style: none;
}
&#10;#rhiiucptef p {
  margin: 0;
  padding: 0;
}
&#10;#rhiiucptef .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: 0;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rhiiucptef .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#rhiiucptef .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 1px;
  padding-bottom: 3px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#rhiiucptef .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 2.5px;
  padding-bottom: 3.5px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#rhiiucptef .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#rhiiucptef .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rhiiucptef .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rhiiucptef .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 2.5px;
  padding-bottom: 2.5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#rhiiucptef .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rhiiucptef .gt_group_heading {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#rhiiucptef .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#rhiiucptef .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rhiiucptef .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rhiiucptef .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#rhiiucptef .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhiiucptef .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#rhiiucptef .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rhiiucptef .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rhiiucptef .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhiiucptef .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rhiiucptef .gt_last_summary_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhiiucptef .gt_first_grand_summary_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_last_grand_summary_row_top {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rhiiucptef .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhiiucptef .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rhiiucptef .gt_sourcenote {
  font-size: 90%;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhiiucptef .gt_left {
  text-align: left;
}
&#10;#rhiiucptef .gt_center {
  text-align: center;
}
&#10;#rhiiucptef .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rhiiucptef .gt_font_normal {
  font-weight: normal;
}
&#10;#rhiiucptef .gt_font_bold {
  font-weight: bold;
}
&#10;#rhiiucptef .gt_font_italic {
  font-style: italic;
}
&#10;#rhiiucptef .gt_super {
  font-size: 65%;
}
&#10;#rhiiucptef .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rhiiucptef .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rhiiucptef .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rhiiucptef .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rhiiucptef .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rhiiucptef .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rhiiucptef .gt_indent_5 {
  text-indent: 25px;
}
&#10;#rhiiucptef .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#rhiiucptef div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>wt</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="value">value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean">mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf.low">conf.low</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf.high">conf.high</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="null">null</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="t.value">t.value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="df">df</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="p.value">p.value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="footnote">footnote</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="9" class="gt_group_heading" scope="colgroup" id="factor(am)">factor(am)</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="factor(am)  value" class="gt_row gt_left">0</td>
<td headers="factor(am)  mean" class="gt_row gt_right">3.8</td>
<td headers="factor(am)  conf.low" class="gt_row gt_right">3.4</td>
<td headers="factor(am)  conf.high" class="gt_row gt_right">4.1</td>
<td headers="factor(am)  null" class="gt_row gt_right"><br /></td>
<td headers="factor(am)  t.value" class="gt_row gt_right"><br /></td>
<td headers="factor(am)  df" class="gt_row gt_right"><br /></td>
<td headers="factor(am)  p.value" class="gt_row gt_left"><br /></td>
<td headers="factor(am)  footnote" class="gt_row gt_right"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span> </td></tr>
    <tr><td headers="factor(am)  value" class="gt_row gt_left">1</td>
<td headers="factor(am)  mean" class="gt_row gt_right">2.4</td>
<td headers="factor(am)  conf.low" class="gt_row gt_right">2.0</td>
<td headers="factor(am)  conf.high" class="gt_row gt_right">2.8</td>
<td headers="factor(am)  null" class="gt_row gt_right"><br /></td>
<td headers="factor(am)  t.value" class="gt_row gt_right"><br /></td>
<td headers="factor(am)  df" class="gt_row gt_right"><br /></td>
<td headers="factor(am)  p.value" class="gt_row gt_left"><br /></td>
<td headers="factor(am)  footnote" class="gt_row gt_right"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span> </td></tr>
    <tr><td headers="factor(am)  value" class="gt_row gt_left">automatic - manual</td>
<td headers="factor(am)  mean" class="gt_row gt_right">1.4</td>
<td headers="factor(am)  conf.low" class="gt_row gt_right">0.85</td>
<td headers="factor(am)  conf.high" class="gt_row gt_right">1.9</td>
<td headers="factor(am)  null" class="gt_row gt_right">0</td>
<td headers="factor(am)  t.value" class="gt_row gt_right">5.5</td>
<td headers="factor(am)  df" class="gt_row gt_right">29</td>
<td headers="factor(am)  p.value" class="gt_row gt_left">< 0.0001</td>
<td headers="factor(am)  footnote" class="gt_row gt_right"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>2</sup></span> </td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span> One Sample t-test (two.sided), with 95% confidence intervals.</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>2</sup></span> Welch Two Sample t-test (two.sided), with 95% confidence intervals.</td>
    </tr>
  </tfoot>
</table>
</div>

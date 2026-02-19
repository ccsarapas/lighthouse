# Count observations with totals row

This variant of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
adds a row with column totals. Totals are computed for the first column
passed to `...` unless otherwise specified in `totals_for`.

## Usage

``` r
count_with_total(
  .data,
  ...,
  totals_for = NULL,
  label = "Total",
  first_row = FALSE,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = dplyr::group_by_drop_default()
)
```

## Arguments

- .data:

  A data frame.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to group by.

- totals_for:

  The variable to total for. If omitted, defaults to the first variable
  in `...`.

- label:

  The label for the totals row. Defaults to "Total".

- first_row:

  If `TRUE`, the totals row will be placed first in the output. If
  `FALSE` (default), it will be placed last.

- wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Frequency weights. Can be `NULL` or a variable:

  - If `NULL` (the default), counts the number of rows in each group.

  - If a variable, computes `sum(wt)` for each group.

- sort:

  If `TRUE`, will show the largest groups at the top.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- .drop:

  Handling of factor levels that don't appear in the data, passed on to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  If `FALSE` will include counts for empty groups (i.e. for levels of
  factors that don't exist in the data).

## Value

A data frame with counts for each grouping level, along with a "totals"
row with the column totals for the totaled variable.

## Examples

``` r
mtcars %>%
  count_with_total(cyl)
#>     cyl  n
#> 1     4 11
#> 2     6  7
#> 3     8 14
#> 4 Total 32

mtcars %>%
  count_with_total(cyl, gear)
#>      cyl gear  n
#> 1      4    3  1
#> 2      4    4  8
#> 3      4    5  2
#> 4      6    3  2
#> 5      6    4  4
#> 6      6    5  1
#> 7      8    3 12
#> 8      8    5  2
#> 9  Total    3 15
#> 10 Total    4 12
#> 11 Total    5  5

mtcars %>%
  count_with_total(cyl, gear, totals_for = gear)
#>    cyl  gear  n
#> 1    4     3  1
#> 2    4     4  8
#> 3    4     5  2
#> 4    4 Total 11
#> 5    6     3  2
#> 6    6     4  4
#> 7    6     5  1
#> 8    6 Total  7
#> 9    8     3 12
#> 10   8     5  2
#> 11   8 Total 14
```

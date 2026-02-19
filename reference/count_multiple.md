# Count observations for multiple variables

This variant of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
returns frequencies and (optionally) proportions for each column passed
to `...`.

## Usage

``` r
count_multiple(
  .data,
  ...,
  .pct = TRUE,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  na.rm = FALSE,
  .by = NULL,
  .drop = TRUE
)
```

## Arguments

- .data:

  A data frame.

- ...:

  Columns to count frequencies for. Can be named expressions.

- .pct:

  If `TRUE` (default), include percentages.

- sort:

  If `TRUE`, sort output by frequency.

- name:

  Name for the frequency column. Default is \\n\\.

- na.rm:

  If `TRUE`, remove rows with `NA` values.

- .by:

  a selection of columns to group by for just this operation,
  functioning as an alternative to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  Percentages will be computed within each group rather than for the
  grand total. See examples.

- .drop:

  Handling of factor levels that don't appear in the data, passed on to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  If `FALSE` will include counts for empty groups (i.e. for levels of
  factors that don't exist in the data).

## Value

A data frame with columns:

- Any grouping variables from the input `data` or specified in `.by`.

- `Variable`: The name of the column being counted.

- `Value`: The unique values from the counted column.

- `n`: The frequency of each unique value.

- `pct`: (If `.pct = TRUE`) The percentage each count represents within
  its variable.

## Examples

``` r
iris %>%
  count_multiple(Species, Sepal.Length > 5)
#> # A tibble: 5 × 4
#>   Variable         Value          n   pct
#>   <chr>            <fct>      <int> <dbl>
#> 1 Species          setosa        50 0.333
#> 2 Species          versicolor    50 0.333
#> 3 Species          virginica     50 0.333
#> 4 Sepal.Length > 5 FALSE         32 0.213
#> 5 Sepal.Length > 5 TRUE         118 0.787

## note effects of grouping
# no grouping
ggplot2::mpg %>%
  count_multiple(year, drv, cyl)
#> # A tibble: 9 × 4
#>   Variable Value     n    pct
#>   <chr>    <fct> <int>  <dbl>
#> 1 year     1999    117 0.5   
#> 2 year     2008    117 0.5   
#> 3 drv      4       103 0.440 
#> 4 drv      f       106 0.453 
#> 5 drv      r        25 0.107 
#> 6 cyl      4        81 0.346 
#> 7 cyl      5         4 0.0171
#> 8 cyl      6        79 0.338 
#> 9 cyl      8        70 0.299 

# grouping w `group_by()`: counts and % nested within groups, output is grouped
ggplot2::mpg %>%
  dplyr::group_by(year) %>%
  count_multiple(drv, cyl)
#> # A tibble: 13 × 5
#> # Groups:   year [2]
#>     year Variable Value     n    pct
#>    <int> <chr>    <fct> <int>  <dbl>
#>  1  1999 drv      4        49 0.419 
#>  2  1999 drv      f        57 0.487 
#>  3  1999 drv      r        11 0.0940
#>  4  2008 drv      4        54 0.462 
#>  5  2008 drv      f        49 0.419 
#>  6  2008 drv      r        14 0.120 
#>  7  1999 cyl      4        45 0.385 
#>  8  1999 cyl      6        45 0.385 
#>  9  1999 cyl      8        27 0.231 
#> 10  2008 cyl      4        36 0.308 
#> 11  2008 cyl      5         4 0.0342
#> 12  2008 cyl      6        34 0.291 
#> 13  2008 cyl      8        43 0.368 

# grouping w `.by`: counts and % nested within groups, output isn't grouped
ggplot2::mpg %>%
  count_multiple(drv, cyl, .by = year)
#> # A tibble: 13 × 5
#>     year Variable Value     n    pct
#>    <int> <chr>    <fct> <int>  <dbl>
#>  1  1999 drv      4        49 0.419 
#>  2  1999 drv      f        57 0.487 
#>  3  1999 drv      r        11 0.0940
#>  4  2008 drv      4        54 0.462 
#>  5  2008 drv      f        49 0.419 
#>  6  2008 drv      r        14 0.120 
#>  7  1999 cyl      4        45 0.385 
#>  8  1999 cyl      6        45 0.385 
#>  9  1999 cyl      8        27 0.231 
#> 10  2008 cyl      4        36 0.308 
#> 11  2008 cyl      5         4 0.0342
#> 12  2008 cyl      6        34 0.291 
#> 13  2008 cyl      8        43 0.368 
```

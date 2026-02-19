# Count observations with percentage

This variant of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
includes a column showing percentage of total observations for each
group.

## Usage

``` r
count_pct(
  .data,
  ...,
  na.rm = FALSE,
  .by = NULL,
  wt = NULL,
  sort = FALSE,
  .drop = dplyr::group_by_drop_default()
)
```

## Arguments

- .data:

  A data frame. \#' @param ... Variables to group by. Will be passed to
  [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to group by.

- na.rm:

  If `TRUE`, removes rows with `NA` values before calculations.

- .by:

  a selection of columns to group by for just this operation,
  functioning as an alternative to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  Percentages will be computed within each group rather than for the
  grand total. See examples.

- wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Frequency weights. Can be `NULL` or a variable:

  - If `NULL` (the default), counts the number of rows in each group.

  - If a variable, computes `sum(wt)` for each group.

- sort:

  If `TRUE`, will show the largest groups at the top.

- .drop:

  Handling of factor levels that don't appear in the data, passed on to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  If `FALSE` will include counts for empty groups (i.e. for levels of
  factors that don't exist in the data).

## Value

A data frame with columns for grouping variables, `n` (the count of
observations in each group), and `pct` (the percentage of total
observations in each group).

## Details

Percentages within subgroups can be obtained by grouping with `group_by`

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

## note effect of `na.rm` on percentages
dplyr::starwars %>%
  count_pct(gender)
#> # A tibble: 3 × 3
#>   gender        n    pct
#>   <chr>     <int>  <dbl>
#> 1 feminine     17 0.195 
#> 2 masculine    66 0.759 
#> 3 NA            4 0.0460

dplyr::starwars %>%
  count_pct(gender, na.rm = TRUE)
#> # A tibble: 2 × 3
#>   gender        n   pct
#>   <chr>     <int> <dbl>
#> 1 feminine     17 0.205
#> 2 masculine    66 0.795

## note effect of grouping on percentages
# no grouping: % of grand total
ggplot2::mpg %>%
 count_pct(year, cyl)
#> # A tibble: 7 × 4
#>    year   cyl     n    pct
#>   <int> <int> <int>  <dbl>
#> 1  1999     4    45 0.192 
#> 2  1999     6    45 0.192 
#> 3  1999     8    27 0.115 
#> 4  2008     4    36 0.154 
#> 5  2008     5     4 0.0171
#> 6  2008     6    34 0.145 
#> 7  2008     8    43 0.184 

# grouping w `group_by()`: % of group, output is grouped
ggplot2::mpg %>%
  dplyr::group_by(year) %>%
  count_pct(cyl)
#> # A tibble: 7 × 4
#> # Groups:   year [2]
#>    year   cyl     n    pct
#>   <int> <int> <int>  <dbl>
#> 1  1999     4    45 0.385 
#> 2  1999     6    45 0.385 
#> 3  1999     8    27 0.231 
#> 4  2008     4    36 0.308 
#> 5  2008     5     4 0.0342
#> 6  2008     6    34 0.291 
#> 7  2008     8    43 0.368 

# grouping w `.by`: % of group, output isn't grouped
ggplot2::mpg %>%
  count_pct(cyl, .by = year)
#> # A tibble: 7 × 4
#>    year   cyl     n    pct
#>   <int> <int> <int>  <dbl>
#> 1  1999     4    45 0.385 
#> 2  1999     6    45 0.385 
#> 3  1999     8    27 0.231 
#> 4  2008     4    36 0.308 
#> 5  2008     5     4 0.0342
#> 6  2008     6    34 0.291 
#> 7  2008     8    43 0.368 
```

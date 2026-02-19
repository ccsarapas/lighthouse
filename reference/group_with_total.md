# Add "Total" group to grouped dataframe

Groups a dataframe by columns specified in `...` using
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
and adds an additional group containing all observations. Useful for
including a "total" or "overall" row in summaries.

If more than one column is passed to `...`, the "total" group will
combine all groups in the first column passed, unless a different column
is specified in `.totals_for`.

Removing or changing the grouping structure after calling
`group_with_total()` but before aggregating may yield inaccurate
results.

## Usage

``` r
group_with_total(
  .data,
  ...,
  .totals_for = NULL,
  .label = "Total",
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data),
  .first_row = FALSE
)
```

## Examples

``` r
ggplot2::mpg %>%
  group_with_total(class) %>%
  dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))
#> # A tibble: 8 × 4
#>   class          n   cty   hwy
#>   <fct>      <int> <dbl> <dbl>
#> 1 2seater        5  15.4  24.8
#> 2 compact       47  20.1  28.3
#> 3 midsize       41  18.8  27.3
#> 4 minivan       11  15.8  22.4
#> 5 pickup        33  13    16.9
#> 6 subcompact    35  20.4  28.1
#> 7 suv           62  13.5  18.1
#> 8 Total        234  16.9  23.4

ggplot2::mpg %>%
  group_with_total(year, drv, .label = "all years") %>%
  dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by year and drv.
#> ℹ Output is grouped by year.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(year, drv))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
#> # A tibble: 9 × 5
#> # Groups:   year [3]
#>   year      drv       n   cty   hwy
#>   <fct>     <chr> <int> <dbl> <dbl>
#> 1 1999      4        49  14.2  18.8
#> 2 1999      f        57  20.0  27.9
#> 3 1999      r        11  14    20.6
#> 4 2008      4        54  14.4  19.5
#> 5 2008      f        49  20.0  28.4
#> 6 2008      r        14  14.1  21.3
#> 7 all years 4       103  14.3  19.2
#> 8 all years f       106  20.0  28.2
#> 9 all years r        25  14.1  21  

ggplot2::mpg %>%
  group_with_total(year, drv, .totals_for = drv) %>%
  dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by year and drv.
#> ℹ Output is grouped by year.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(year, drv))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
#> # A tibble: 8 × 5
#> # Groups:   year [2]
#>    year drv       n   cty   hwy
#>   <int> <fct> <int> <dbl> <dbl>
#> 1  1999 4        49  14.2  18.8
#> 2  1999 f        57  20.0  27.9
#> 3  1999 r        11  14    20.6
#> 4  1999 Total   117  17.0  23.4
#> 5  2008 4        54  14.4  19.5
#> 6  2008 f        49  20.0  28.4
#> 7  2008 r        14  14.1  21.3
#> 8  2008 Total   117  16.7  23.5
```

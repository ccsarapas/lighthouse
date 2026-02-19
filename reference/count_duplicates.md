# Count duplicates across specified columns

This variant of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
returns the number of duplicate observations across the specified
columns. Returns the number of unique duplicated values, as well as the
total number of duplicated observations.

## Usage

``` r
count_duplicates(.data, ..., na.rm = FALSE)
```

## Arguments

- .data:

  A data frame.

- ...:

  Columns to use for duplicate checks. If empty, all columns are used.

- na.rm:

  If `TRUE`, rows containing `NA` in the specified columns will be
  removed before counting duplicates.

## Value

A data frame with columns:

- `instances`: The number of times each unique value is duplicated

- `n_unique`: The number of unique values duplicated `instances` times

- `n_total`: The total number of observations duplicated `instances`
  times

## Examples

``` r
df <- tibble::tibble(
  x = c(1, 1, 2, 3, 3),
  y = c('a', 'a', 'b', 'c', 'c')
)

count_duplicates(df)
#> # A tibble: 1 × 3
#>   instances n_unique n_total
#>       <int>    <int>   <int>
#> 1         5        1       5
count_duplicates(df, x)
#> # A tibble: 2 × 3
#>   instances n_unique n_total
#>       <int>    <int>   <int>
#> 1         1        1       1
#> 2         2        2       4
count_duplicates(df, y)
#> # A tibble: 2 × 3
#>   instances n_unique n_total
#>       <int>    <int>   <int>
#> 1         1        1       1
#> 2         2        2       4
```

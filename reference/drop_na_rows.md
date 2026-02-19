# Drop rows where all columns are `NA`

Drops rows where all specified columns are `NA`. If no columns are
specified, all columns will be considered.

## Usage

``` r
drop_na_rows(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  (Optional) Columns to test for `NA`s. If not specified, all columns
  will be considered.

## Value

A data frame with rows removed that contain `NA` values across all
(specified) columns.

## Examples

``` r
dat <- tibble::tibble(
  x = c(NA, NA, 3),
  y = c(NA, NA, 4),
  z = c(5, NA, NA)
)
drop_na_rows(dat)
#> # A tibble: 2 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1    NA    NA     5
#> 2     3     4    NA
drop_na_rows(dat, x, y)
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     3     4    NA
```

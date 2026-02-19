# Count unique values in data frame columns

This variant of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
returns the number of unique values across a set of columns in a data
frame.

## Usage

``` r
count_unique(.data, ..., name = "n_unique", na.rm = FALSE)
```

## Arguments

- .data:

  a data frame.

- ...:

  columns to count unique values across.

- name:

  name to give the unique count column.

- na.rm:

  exclude `NA`s from counts?

## Examples

``` r
mtcars %>%
  count_unique(cyl, gear)
#>   n_unique
#> 1        8

mtcars %>%
  count_unique(cyl, gear, carb, name = "unique_combos")
#>   unique_combos
#> 1            12
```

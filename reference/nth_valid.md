# Get the nth, first, or last non-`NA` value in a vector

These functions retrieve the nth, first or last non-`NA` value in a
vector. If there are fewer than `n` non-`NA` values, a default value can
be returned.

## Usage

``` r
nth_valid(x, n, default = NA)

first_valid(x, default = NA)

last_valid(x, default = NA)
```

## Arguments

- x:

  A vector.

- n:

  integer. Position of non-`NA` value to return. Negative values start
  from the end of the vector.

- default:

  a default value to use if there are fewer than `n` non-`NA` values in
  `x`. Will be cast to the type of `x`.

## Value

- `nth_valid`: The nth non-`NA` value in `x`.

- `first_valid`: The first non-`NA` value in `x`.

- `last_valid`: The last non-`NA` value in `x`.

## See also

[`dplyr::nth()`](https://dplyr.tidyverse.org/reference/nth.html)

## Examples

``` r
x <- c(NA, 7, NA, 5, 4, NA, 2, NA)

first_valid(x)
#> [1] 7
last_valid(x)
#> [1] 2

nth_valid(x, 2)
#> [1] 5
nth_valid(x, -2)
#> [1] 4

nth_valid(x, 6)
#> [1] NA
nth_valid(x, 6, default = -Inf)
#> [1] -Inf
```

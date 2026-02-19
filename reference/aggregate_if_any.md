# Sums, maxima and minima with alternative missing value handling

Returns the sum, maximum, or minimum of input values, similar to
[`base::sum()`](https://rdrr.io/r/base/sum.html),
[`min()`](https://rdrr.io/r/base/Extremes.html), and
[`max()`](https://rdrr.io/r/base/Extremes.html). Unlike these base
functions, these variants return `NA` when all values are `NA` and
`na.rm = TRUE`. ([`base::sum()`](https://rdrr.io/r/base/sum.html),
[`min()`](https://rdrr.io/r/base/Extremes.html), and
[`max()`](https://rdrr.io/r/base/Extremes.html) return `0`, `-Inf`, and
`Inf`, respectively, in this situation). Also unlike base functions,
`na.rm` is `TRUE` by default (since this is the typical use case).

## Usage

``` r
sum_if_any(..., na.rm = TRUE)

max_if_any(..., na.rm = TRUE)

min_if_any(..., na.rm = TRUE)
```

## Arguments

- ...:

  numeric, logical, or (for `max_if_any()` and `min_if_any()`) character
  vectors.

- na.rm:

  logical. Should missing values (including NaN) be removed?

## Examples

``` r
some_na <- c(1, 2, NA)
all_na <- c(NA, NA, NA)

# unlike base functions, `na.rm = TRUE` by default
max(some_na)
#> [1] NA
max_if_any(some_na)
#> [1] 2

# unlike base functions, returns `NA` when `na.rm = TRUE` and all inputs are `NA`
sum(all_na, na.rm = TRUE)
#> [1] 0
sum_if_any(all_na)
#> [1] NA
```

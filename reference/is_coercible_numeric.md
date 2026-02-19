# Test for data encoded as other formats

Tests whether each element of a vector can be coerced to another type.
See examples.

## Usage

``` r
is_coercible_numeric(x, all = FALSE, na = c("NA", "TRUE"))

is_coercible_integer(x, all = FALSE, na = c("NA", "TRUE"))

is_coercible_logical(
  x,
  all = FALSE,
  na = c("NA", "TRUE"),
  numeric = c("binary", "any")
)
```

## Arguments

- x:

  Vector to be tested

- all:

  If `TRUE`, returns a single logical indicating whether every element
  of `x` is coercible. If `FALSE` (the default), returns a logical
  vector the same length as `x` testing each element of `x`.

- na:

  Should `NA` values test `NA` (the default) or `TRUE`?

- numeric:

  For `is_coercible_logical`, should any numeric value test `TRUE`, or
  only `0` and `1` (the default)?

## Examples

``` r
x <- c("1", "-1.23", "$1,234", NA)
is_coercible_numeric(x)
#> [1]  TRUE  TRUE FALSE    NA

is_coercible_numeric(x, na = "TRUE")
#> [1]  TRUE  TRUE FALSE  TRUE

is_coercible_numeric(x, all = TRUE)
#> [1] FALSE

is_coercible_integer(x)
#> [1]  TRUE FALSE FALSE    NA

y <- c("TRUE", "T", "F", "YES", "NA", NA)
is_coercible_logical(y)
#> [1]  TRUE  TRUE  TRUE FALSE FALSE    NA

z <- c(0, 1, 2, .1, -1)
is_coercible_logical(z)
#> [1]  TRUE  TRUE FALSE FALSE FALSE

is_coercible_logical(z, numeric = "any")
#> [1] TRUE TRUE TRUE TRUE TRUE
```

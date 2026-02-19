# Vectorized logical tests

`is_TRUE()` and `is_FALSE()` are vectorized versions of
[`base::isTRUE()`](https://rdrr.io/r/base/Logic.html) and
[`base::isFALSE()`](https://rdrr.io/r/base/Logic.html), respectively.
`is_TRUE()` returns `TRUE` for each vector element that evaluates to
`TRUE`, and `FALSE` for all other elements (including for `NA`s and
non-logical values). Useful for handling `NA`s in logical tests.

## Usage

``` r
is_TRUE(x, strict = TRUE)

is_FALSE(x, strict = TRUE)

is_TRUE_or_NA(x, strict = TRUE)

is_FALSE_or_NA(x, strict = TRUE)
```

## Arguments

- x:

  Vector to be tested

- strict:

  If `TRUE` (the default), numeric and character types will always
  return `FALSE`. If `FALSE`, numeric and character vectors that can be
  coerced to logical (e.g., `1`, `"FALSE"`) will be coerced before
  testing.

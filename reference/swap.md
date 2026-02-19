# Swap column values, optionally based on condition

`swap()` swaps values between two columns. `swap_if()` swaps values only
in rows where a condition is met. `x` and `y` will be cast to a common
type using
[`vctrs::vec_cast_common()`](https://vctrs.r-lib.org/reference/vec_cast.html).

## Usage

``` r
swap(.data, x, y)

swap_if(.data, cond, x, y, missing = c("NA", "keep", "swap"))
```

## Arguments

- x:

  column to swap with `y`.

- y:

  column to swap with `x`.

- cond:

  a logical vector.

- missing:

  what to do if `cond` is `NA`.

  - `"NA"` replaces both `x` and `y` with `NA`.

  - `"keep"` keeps values in their original columns (as though `cond`
    were `FALSE`).

  - `"swap"` swaps values between `x` and `y` (as though `cond` were
    `TRUE`).

## References

Based on Hadley's code in a [this dplyr GitHub
issue](https://github.com/tidyverse/dplyr/issues/2149#issuecomment-258916706).

# Median value as double - DEPRECATED

Deprecated in lighthouse 0.7.0. The main use case for this function was
to avoid type errors in
[`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
and
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html),
but this is no longer necessary with [changes introduced in dplyr
v1.1.0](https://dplyr.tidyverse.org/news/index.html#vctrs-1-1-0).

Returns median of `x` as a double vector. Alternative to
[`stats::median()`](https://rdrr.io/r/stats/median.html) when a
consistent return value is needed.

## Usage

``` r
median_dbl(x, na.rm = FALSE, ...)
```

## Examples

``` r
if (FALSE) { # \dontrun{
# stats::median raises error because of inconsistent return types
### note this no longer raises an error with dplyr >= 1.1.0
dplyr::if_else(c(TRUE, FALSE), median(1:4), median(1:5))
# Error in `dplyr::if_else()`:
# ! `false` must be a double vector, not an integer vector.

# dplyr::if_else(c(TRUE, FALSE), median_dbl(1:4), median_dbl(1:5))
# 2.5 3.0
} # }
```

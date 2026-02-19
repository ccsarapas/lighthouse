# Maxima and minima with alternative missing value handling - DEPRECATED

Deprecated in lighthouse 0.7.0 favor of
[`max_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md)
and
[`min_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md),
which these now call.

## Usage

``` r
safe_max(..., na.rm = TRUE)

safe_min(..., na.rm = TRUE)
```

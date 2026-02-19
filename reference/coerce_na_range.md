# Set NA values based on numbers stored as strings - DEPRECATED

Deprecated in lighthouse 0.7.2 favor of
[`na_if_range()`](https://ccsarapas.github.io/lighthouse/reference/na_if_range.md),
which this now calls.

Changes values coercible to numeric in range `range_min`:`range_max` to
`NA`. Useful for imported SPSS files.

## Usage

``` r
coerce_na_range(x, range_min = -Inf, range_max = -1)
```

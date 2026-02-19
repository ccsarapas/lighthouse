# Compute the standard error - DEPRECATED

Deprecated in 0.7.0 favor of the more specific functions
[`se_mean()`](https://ccsarapas.github.io/lighthouse/reference/se_mean.md)
and
[`se_mean()`](https://ccsarapas.github.io/lighthouse/reference/se_mean.md).
`se()` now calls
[`se_mean()`](https://ccsarapas.github.io/lighthouse/reference/se_mean.md)
with a deprecation warning.

Computes the standard error of the values in x.

## Usage

``` r
se(x, na.rm = FALSE)
```

## Arguments

- x:

  a numeric vector or a non-factor object coercible to numeric by
  `as.double(x)`.

- na.rm:

  logical. Should missing values be removed?

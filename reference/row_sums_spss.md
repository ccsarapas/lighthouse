# Replicate SPSS SUM() function - DEPRECATED

Deprecated in lighthouse 0.7.2 in favor of
[`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md).

Sums across columns a la SPSS: `NA`s are counted as 0s, but if ALL
variables are `NA`, then result is `NA`.

## Usage

``` r
row_sums_spss(...)
```

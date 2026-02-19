# Compute Cohen's w

Cohen's w is an effect size measure for associations between nominal
variables, generally used in conjunction with chi-squared tests.
`cohen_w()` computes Cohen's w from results from a chi-squared test.

## Usage

``` r
cohen_w(chisq)
```

## Arguments

- chisq:

  An `"htest"` object returned by
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

## Examples

``` r
chisq_out <- chisq.test(ggplot2::diamonds$cut, ggplot2::diamonds$color)
cohen_w(chisq_out)
#> [1] 0.07584867
```

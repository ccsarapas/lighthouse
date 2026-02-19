# Compute the standard error of a proportion

Computes the standard error of a proportion for a logical or numeric
vector.

## Usage

``` r
se_prop(
  x,
  na.rm = FALSE,
  min_var = 5,
  low_var_action = c("warn", "ignore", "error")
)
```

## Arguments

- x:

  a logical or numeric vector. If numeric, it must include only 0s, 1s,
  and/or `NA`s.

- na.rm:

  logical. Should missing values be removed?

- min_var:

  numeric. Minimum variance (n \* p \* (1 - p)) for valid normal
  approximation of the binomial. See Details.

- low_var_action:

  character. Action to take when variance is below `min_var`.

## Details

Standard error of a proportion is calculated using the formula:

\$\$SE = \sqrt{\frac{p(1 - p)}{n}}\$\$

This formula assumes that the binomial sampling distribution underlying
the observed proportion can be approximated by a normal distribution.
This assumption is valid when the proportion variance (n*p*(1 - p)) is
sufficiently large, but may not hold when variance is lower. `se_prop()`
therefore issues a warning when the variance is less than 5; this
behavior can be overridden using the `min_var` and `low_var_action`
arguments.

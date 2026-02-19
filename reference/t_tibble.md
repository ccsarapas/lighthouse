# Transpose a tibble

Given a tibble or data.frame `x`, returns the transpose of `x`. Similar
to [`base::t()`](https://rdrr.io/r/base/t.html), but accepts and returns
tibbles, and includes options for row column name handling.

## Usage

``` r
t_tibble(x, names_to = "Variable", names_from = NULL)
```

## Arguments

- x:

  A dataframe or tibble.

- names_to:

  Name for column in the transposed tibble containing column names from
  `x`

- names_from:

  Column in `x` used for column names of transposed tibble. If not
  specified and `x` has rownames, these will be used; otherwise columns
  will be named `...1`, `...2`, etc.

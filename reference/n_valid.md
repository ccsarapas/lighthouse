# Count non-missing cases

`n_valid()` returns the number of vector elements that are not `NA`. It
returns the percentage of non-`NA` values if `out` = `"pct"`, and a
tibble containing number and percentage if `out` = `"n_pct"`.

`pct_valid()` is a wrapper around `n_valid(out = "pct")`.

`n_pct_valid()` is a wrapper around `n_pct_valid(out = "n_pct")`.

## Usage

``` r
n_valid(x, out = c("n", "pct", "n_pct"))

pct_valid(x)

n_pct_valid(x)
```

## Arguments

- x:

  A vector.

- out:

  What should `n_valid()` return? `"n"`, the default, returns the number
  of non-missing values, `"pct"` returns the proportion of non-missing
  values, and `"n_pct"` returns a tibble containing both the number and
  percentage of non-missing values.

## Value

`pct_valid()` returns a single value. `n_valid()` returns a single value
unless `out = "n_pct"`, in which case it returns a tibble.
`n_pct_valid()` returns a tibble.

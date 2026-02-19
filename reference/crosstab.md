# Cross-tabulate observations

Builds a contingency table similar to
[`base::table()`](https://rdrr.io/r/base/table.html). Unlike
[`base::table()`](https://rdrr.io/r/base/table.html), `crosstab()` is
pipe-friendly, and outputs an ordinary tibble / data.frame – e.g., it
will retain its structure if exported to csv.

Currently supports only two variables.

## Usage

``` r
crosstab(.data, ..., .drop = TRUE)
```

## Arguments

- .data:

  A data frame.

- ...:

  Variables to cross-tabulate. (Only two variables are currently
  supported.)

- .drop:

  If `FALSE`, will include counts for empty groups (i.e. for levels of
  factors that don't exist in the data).

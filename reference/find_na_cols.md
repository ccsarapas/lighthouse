# Identify or remove columns with no data

`find_na_cols()` returns the names of all columns in `.data` where all
values are `NA`. `drop_na_cols()` returns the dataset with all-`NA`
columns removed.

## Usage

``` r
find_na_cols(.data, cols = tidyselect::everything())

drop_na_cols(.data, cols = tidyselect::everything(), quietly = FALSE)
```

## Arguments

- .data:

  A data frame or data frame extension (e.g. a tibble).

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to check.

- quietly:

  If `FALSE`, will print which columns were dropped.

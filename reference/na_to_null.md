# Replace NA with NULL and vice versa

`na_to_null()` Replaces all `NA`s in a vector or list with `NULL`. Can
be useful for lists of function arguments (e.g., when using
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)).

`null_to_na()` Replaces all `NULL`s in a list with `NA`s. Returns an
atomic vector if `unlist = TRUE` and a list otherwise.

## Usage

``` r
na_to_null(x)

null_to_na(x, unlist = FALSE)
```

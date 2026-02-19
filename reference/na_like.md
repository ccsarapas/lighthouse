# Generate NA values of appropriate type - DEPRECATED

This function was deprecated in lighthouse 0.7.0 because (1) it's always
been buggy and (2) its main purpose was to pass appropriate `NA`s to
[`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
and
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html),
but this is no longer necessary with [changes introduced in dplyr
v1.1.0](https://dplyr.tidyverse.org/news/index.html#vctrs-1-1-0)

Returns compatible `NA` based on `x`. This is usually of the same type
as `x` (e.g., `NA_real_` if `x` is a double vector). If `x` is a factor,
will return `NA_character_` if `factor_as_character = TRUE` (the
default) and `NA_integer_` otherwise.

## Usage

``` r
na_like(x, factor_as_character = TRUE, match_length = FALSE)
```

# Results of case_when() as factor.

Wrapper for
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html),
but result is a factor with levels in same order passed to `...`.
Returns an ordered factor if `.ordered` is `TRUE`.

## Usage

``` r
fct_case_when(..., .ordered = FALSE)
```

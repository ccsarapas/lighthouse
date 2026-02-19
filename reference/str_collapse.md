# Collapse a character vector into a single string

Concatenates all elements in a character vector. Essentially a
column-wise variant of dplyr::str_c(). Can accept multiple character
vectors, which are returned as separate character strings if `join` =
NULL (the default), or concatenated with a separator supplied to `join`.

## Usage

``` r
str_collapse(..., sep = "", join = NULL)
```

## Arguments

- ...:

  one or more character vectors

- sep:

  string to insert between elements of each input vector

- join:

  optional string to combine output into a single string. join multiple
  collapsed vectors. If NULL (default), returns a character vector with
  each input vector collapsed separately.

## Value

If `join` is NULL, returns a character vector with length equal to the
number of vectors passed to `...`. If `join` is provided, returns a
character vector of length 1.

## Details

When only a single character vector is passed to `...`, behavior will be
similar to
[`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html)
using the `collapse` argument. But behavior differs when multiple
vectors are passed to `...` – see examples. The difference arise because
`str_collapse()` first collapses each vector, then optionally joins the
resulting vectors, whereas
[`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html)
first joins across vectors before collapsing the resulting vector.

## Examples

``` r
# with just a single character vector, behavior is similar to `stringr::str_c()`
# with the `collapse` argument
abc <- c("a", "b", "c")
str_collapse(abc, sep = "-")
#> [1] "a-b-c"
stringr::str_c(abc, collapse = "-")
#> [1] "a-b-c"

# but behavior differs when multiple vectors are passed
def <- c("d", "e", "f")
str_collapse(abc, def, sep = "-")
#> [1] "a-b-c" "d-e-f"
stringr::str_c(abc, def, collapse = "-")
#> [1] "ad-be-cf"

str_collapse(abc, def, sep = "-", join = " | ")
#> [1] "a-b-c | d-e-f"
stringr::str_c(abc, def, collapse = "-", sep = " | ")
#> [1] "a | d-b | e-c | f"
stringr::str_c(abc, def, sep = "-", collapse = " | ")
#> [1] "a-d | b-e | c-f"

# can accept vectors of different lengths
lmnop <- c("l", "m", "n", "o", "p")
str_collapse(abc, def, lmnop, join = ", ")
#> [1] "abc, def, lmnop"
```

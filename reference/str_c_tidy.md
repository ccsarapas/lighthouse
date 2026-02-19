# Tidy string concatenation

This function performs a tidyverse-friendly string concatenation. It
takes a data frame or tibble and a selection of columns, concatenates
the string values in each row, and returns the concatenated strings as a
vector.

## Usage

``` r
str_c_tidy(
  ...,
  sep = "",
  collapse = NULL,
  na.rm = FALSE,
  if_all_na = c("empty", "NA")
)
```

## Arguments

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  tidyselect expression indicating character columns vectors or columns
  coercible to character.

- sep:

  separator to insert between input vectors.

- collapse:

  optional character string to combine results into a single string.

- na.rm:

  logical. Remove missing values before concatenating? Treatment of
  `NA`s is similar to
  [`str_c_narm()`](https://ccsarapas.github.io/lighthouse/reference/str_c_narm.md)
  and differs from behavior of
  [`paste()`](https://rdrr.io/r/base/paste.html) and
  [`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html).
  See Details of
  [`str_c_narm()`](https://ccsarapas.github.io/lighthouse/reference/str_c_narm.md).

- if_all_na:

  what to do if `na.rm = TRUE` and *all* values in a row are `NA`.
  `"empty"`, the default, returns an empty string. `"NA"` returns `NA`.
  Ignored if `na.rm = FALSE`.

## Value

A character vector.

## See also

[`str_c_narm`](https://ccsarapas.github.io/lighthouse/reference/str_c_narm.md)

## Examples

``` r
df <- tibble::tribble(
  ~x, ~y, ~z,
  "a", "b", "c",
  "d", NA, "f",
  "g", "h", NA
)

df %>% dplyr::mutate(combined = str_c_tidy(x:z))
#> # A tibble: 3 × 4
#>   x     y     z     combined
#>   <chr> <chr> <chr> <chr>   
#> 1 a     b     c     abc     
#> 2 d     NA    f     NA      
#> 3 g     h     NA    NA      
df %>% dplyr::mutate(combined = str_c_tidy(x:z, na.rm = TRUE))
#> # A tibble: 3 × 4
#>   x     y     z     combined
#>   <chr> <chr> <chr> <chr>   
#> 1 a     b     c     abc     
#> 2 d     NA    f     df      
#> 3 g     h     NA    gh      
```

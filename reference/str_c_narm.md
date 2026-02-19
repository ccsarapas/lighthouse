# Concatenate strings with `NA` handling

`str_c_narm` concatenates strings similar to
[`base::paste()`](https://rdrr.io/r/base/paste.html) or
[`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html),
but with different `NA` handling. `NA`s are dropped row-wise prior to
concatenation. See Details and Examples.

## Usage

``` r
str_c_narm(
  ...,
  sep = "",
  collapse = NULL,
  if_all_na = c("empty", "NA", "remove")
)
```

## Arguments

- ...:

  character vectors or vectors coercible to character. May also be a
  single data frame (to accommodate
  [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html)
  and [`pick()`](https://dplyr.tidyverse.org/reference/pick.html)).

- sep:

  separator to insert between input vectors.

- collapse:

  optional character string to combine results into a single string.

- if_all_na:

  what to do if *all* values in a row are `NA`:

  - `"empty"`: (default) returns an empty string.

  - `"NA"`: returns `NA`.

  - `"remove"`: removes the row from the output.

## Value

A character vector.

## Details

`str_c_narm()` provides alternative missing value handling compared to
[`base::paste()`](https://rdrr.io/r/base/paste.html) and
[`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html).
Given some vectors with missing values:

    #> (dat <- tibble::tibble(v1 = c("a1", "b1", "c1", NA),
                          v2 = c("a2", NA, "c2", NA),
                          v3 = c("a3", "b3", NA, NA)))
    # A tibble: 4 × 3
      v1    v2    v3
      <chr> <chr> <chr>
    1 a1    a2    a3
    2 b1    NA    b3
    3 c1    c2    NA
    4 NA    NA    NA

[`paste()`](https://rdrr.io/r/base/paste.html) and
[`paste0()`](https://rdrr.io/r/base/paste.html) convert `NA`s to the
string `"NA"` before concatenating:

    #> dplyr::mutate(dat, paste = paste(v1, v2, v3, sep = " | "))
    # A tibble: 4 × 4
      v1    v2    v3    paste
      <chr> <chr> <chr> <chr>
    1 a1    a2    a3    a1 | a2 | a3
    2 b1    NA    b3    b1 | NA | b3
    3 c1    c2    NA    c1 | c2 | NA
    4 NA    NA    NA    NA | NA | NA

...while
[`stringr::str_c`](https://stringr.tidyverse.org/reference/str_c.html)
returns `NA` if any value in a row is `NA`:

    #> dplyr::mutate(dat, str_c = stringr::str_c(v1, v2, v3, sep = " | "))
    # A tibble: 4 × 4
      v1    v2    v3    str_c
      <chr> <chr> <chr> <chr>
    1 a1    a2    a3    a1 | a2 | a3
    2 b1    NA    b3    NA
    3 c1    c2    NA    NA
    4 NA    NA    NA    NA

In contrast, `str_c_narm()` removes `NAs` from each row before
concatenating:

    #> dplyr::mutate(dat, str_c_narm = str_c_narm(v1, v2, v3, sep = " | "))
    # A tibble: 4 × 4
      v1    v2    v3    str_c_narm
      <chr> <chr> <chr> <chr>
    1 a1    a2    a3    "a1 | a2 | a3"
    2 b1    NA    b3    "b1 | b3"
    3 c1    c2    NA    "c1 | c2"
    4 NA    NA    NA    ""

...with options for the case where all values in a row are `NA`:

    #> dplyr::mutate(dat, str_c_narm = str_c_narm(v1, v2, v3, sep = " | ", if_all_na = "NA"))
    # A tibble: 4 × 4
      v1    v2    v3    str_c_narm
      <chr> <chr> <chr> <chr>
    1 a1    a2    a3    a1 | a2 | a3
    2 b1    NA    b3    b1 | b3
    3 c1    c2    NA    c1 | c2
    4 NA    NA    NA    NA

## See also

[`str_c_tidy`](https://ccsarapas.github.io/lighthouse/reference/str_c_tidy.md)

## Examples

``` r
str_c_narm(c("a", NA), c("b", "c"), sep = "_")
#> [1] "a_b" "c"  
str_c_narm(c("a", NA), c("b", NA), if_all_na = "NA")
#> [1] "ab" NA  

# compare behavior to `paste()` and `str_c()`
dat <- tibble::tibble(v1 = c("a1", "b1", "c1", NA),
                      v2 = c("a2", NA, "c2", NA),
                      v3 = c("a3", "b3", NA, NA))

dplyr::mutate(
  dat,
  paste = paste(v1, v2, v3, sep = " | "),
  str_c = stringr::str_c(v1, v2, v3, sep = " | "),
  str_c_narm = str_c_narm(v1, v2, v3, sep = " | ")
)
#> # A tibble: 4 × 6
#>   v1    v2    v3    paste        str_c        str_c_narm    
#>   <chr> <chr> <chr> <chr>        <chr>        <chr>         
#> 1 a1    a2    a3    a1 | a2 | a3 a1 | a2 | a3 "a1 | a2 | a3"
#> 2 b1    NA    b3    b1 | NA | b3 NA           "b1 | b3"     
#> 3 c1    c2    NA    c1 | c2 | NA NA           "c1 | c2"     
#> 4 NA    NA    NA    NA | NA | NA NA           ""            
```

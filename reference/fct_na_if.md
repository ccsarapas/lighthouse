# Convert specified factor levels to `NA`

Converts specified level(s) of a factor to `NA`, removing those levels
from the factor. This differs from the behavior of
[`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html),
which (1) replaces values with `NA` but retains the associated factor
level, and (2) can replace only a single value.

## Usage

``` r
fct_na_if(x, y)
```

## Arguments

- x:

  A factor.

- y:

  A character vector of levels to convert to `NA`.

## Value

The input factor with specified levels converted to `NA` and removed
from the levels.

## See also

[`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)

## Examples

``` r
f <- factor(c("a", "b", "c", "a"))
fct_na_if(f, "a")
#> [1] <NA> b    c    <NA>
#> Levels: b c
fct_na_if(f, c("a", "c"))
#> [1] <NA> b    <NA> <NA>
#> Levels: b

# compare `na_if()`
dplyr::na_if(f, "a")
#> [1] <NA> b    c    <NA>
#> Levels: a b c
```

# Compare two data frames and show differences

Given two data frames with the same dimensions and column order, returns
a data frame including only rows and columns with differences.

## Usage

``` r
df_compare(x, y, suffix = c(".x", ".y"), keep = NULL)
```

## Arguments

- x, y:

  a pair of data frames

- suffix:

  suffixes to indicate source data frame in output.

- keep:

  \<\[`tidy-select`\]\[dplyr_tidy_select\> Columns to include in the
  output even if they have no differences.

## Value

A data frame with rows and columns that have differing values between
`x` and `y`. Differing columns are included twice, with suffixes
appended. Columns specified in `keep` are always included.

## Examples

``` r
x <- data.frame(id = 1:3, A = c(7, 8, 9), B = c(10, 20, 30), C = c("x", "y", "z"))
y <- data.frame(id = 1:3, A = c(7, 8, 99), B = c(10, 20, 30), C = c("X", "y", "Z"))

df_compare(x, y)
#>   A.x A.y C.x C.y
#> 1   7   7   x   X
#> 2   9  99   z   Z
df_compare(x, y, keep = id)
#>   id A.x A.y C.x C.y
#> 1  1   7   7   x   X
#> 2  3   9  99   z   Z
```

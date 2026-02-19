# Add empty rows at specified values in a column

Adds empty row(s) based on specified value(s) in `col`. By default, will
insert one empty row after the last occurrence in `col` of each value
passed to `vals`.

## Usage

``` r
add_rows_at_value(
  .data,
  col,
  vals,
  where = c("after_last", "before_first", "after_each", "before_each"),
  no_match = c("error", "warn", "ignore"),
  nrows = 1,
  ...
)
```

## Arguments

- .data:

  a data frame or data frame extension.

- col:

  column to search for values.

- vals:

  character vector of value(s) to search for in `col`.

- where:

  where to insert rows relative to the values in `vals`.

- nrows:

  number of empty rows to insert at each location.

- ...:

  these dots are included to support error-checking and must be empty.

- nomatch:

  what to do if a value in `vals` doesn't appear in `col`.

## Value

An updated version of `.data` with new empty rows inserted.

## Details

In the pre-release version of this function, used in some lighthouse
code, values to search for were passed to `...` as unquoted symbols.
Values must now be now instead be passed to `vals` as a character
vector. The arguments `.where` and `.nrows` have also been renamed to
`where` and `nrows`. The function will attempt to detect and give an
informative warning if it is called with "old" parameters (e.g., with a
deprecated argument or with a list of symbols rather than a character
vector). Also see
[`syms_to_chr()`](https://ccsarapas.github.io/lighthouse/reference/syms_to_chr.md),
which is provided as a utility for adapting old code.

## Examples

``` r
set.seed(13)
ex_data <- tibble::tibble(
  category = sort(sample(LETTERS[1:3], 10, replace = TRUE)),
  var = round(runif(10), 2)
)

add_rows_at_value(ex_data, category, c("A", "B"))
#> # A tibble: 12 × 2
#>    category   var
#>    <chr>    <dbl>
#>  1 A         0.87
#>  2 A         0.68
#>  3 A         0.14
#>  4 A         0.55
#>  5 NA       NA   
#>  6 B         0.68
#>  7 B         0.53
#>  8 B         0.09
#>  9 B         0.62
#> 10 NA       NA   
#> 11 C         0.03
#> 12 C         0.46

add_rows_at_value(ex_data, category, c("A", "C"), where = "after_each")
#> # A tibble: 16 × 2
#>    category   var
#>    <chr>    <dbl>
#>  1 A         0.87
#>  2 NA       NA   
#>  3 A         0.68
#>  4 NA       NA   
#>  5 A         0.14
#>  6 NA       NA   
#>  7 A         0.55
#>  8 NA       NA   
#>  9 B         0.68
#> 10 B         0.53
#> 11 B         0.09
#> 12 B         0.62
#> 13 C         0.03
#> 14 NA       NA   
#> 15 C         0.46
#> 16 NA       NA   

add_rows_at_value(
  ex_data,
  category,
  unique(ex_data$category),
  where = "before_first",
  nrows = 2
)
#> # A tibble: 16 × 2
#>    category   var
#>    <chr>    <dbl>
#>  1 NA       NA   
#>  2 NA       NA   
#>  3 A         0.87
#>  4 A         0.68
#>  5 A         0.14
#>  6 A         0.55
#>  7 NA       NA   
#>  8 NA       NA   
#>  9 B         0.68
#> 10 B         0.53
#> 11 B         0.09
#> 12 B         0.62
#> 13 NA       NA   
#> 14 NA       NA   
#> 15 C         0.03
#> 16 C         0.46
```

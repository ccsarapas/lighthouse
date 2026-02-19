# Get information about data frame columns

Returns a summary of each column's class, type, and missing data. If the
data frame was imported from an SPSS .sav file and the \\labelled\\
package is installed, SPSS variable labels will also be included.

## Usage

``` r
cols_info(x, zap_spss = TRUE)
```

## Arguments

- x:

  A data frame.

- zap_spss:

  If `TRUE` (default) and the \\labelled\\ package is available, convert
  SPSS-style labeled columns to standard R columns. Ignored if
  \\labelled\\ is not installed.

## Value

A tibble with a row for each column in `x`, containing:

- `column`: Column name

- `class`: Column class

- `type`: Column type

- `valid_n`: Number of non-missing values

- `valid_pct`: Percentage of non-missing values

- `label`: SPSS variable label (if applicable)

## Examples

``` r
cols_info(dplyr::starwars)
#> # A tibble: 14 × 5
#>    column     class     type      valid_n valid_pct
#>    <chr>      <chr>     <chr>       <dbl>     <dbl>
#>  1 name       character character      87     1    
#>  2 height     integer   integer        81     0.931
#>  3 mass       numeric   double         59     0.678
#>  4 hair_color character character      82     0.943
#>  5 skin_color character character      87     1    
#>  6 eye_color  character character      87     1    
#>  7 birth_year numeric   double         43     0.494
#>  8 sex        character character      83     0.954
#>  9 gender     character character      83     0.954
#> 10 homeworld  character character      77     0.885
#> 11 species    character character      83     0.954
#> 12 films      list      list           87     1    
#> 13 vehicles   list      list           87     1    
#> 14 starships  list      list           87     1    
```

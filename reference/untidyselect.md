# Convert a tidy selection to a vector of column names

Returns all column names selected by a
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
expression as a character vector (default) or a list of symbols (if
`syms` = `TRUE`).

## Usage

``` r
untidyselect(data, selection, syms = FALSE)
```

## Examples

``` r
dplyr::storms %>%
  untidyselect(c(name:hour, category, tidyselect::ends_with("diameter")))
#> [1] "name"                         "year"                        
#> [3] "month"                        "day"                         
#> [5] "hour"                         "category"                    
#> [7] "tropicalstorm_force_diameter" "hurricane_force_diameter"    

 mtcars %>%
   untidyselect(mpg:drat, syms = TRUE)
#> [[1]]
#> mpg
#> 
#> [[2]]
#> cyl
#> 
#> [[3]]
#> disp
#> 
#> [[4]]
#> hp
#> 
#> [[5]]
#> drat
#> 
```

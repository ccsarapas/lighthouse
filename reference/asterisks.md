# Return asterisks corresponding to p-values

Returns asterisks indicating significance levels for a vector of p
values.

## Usage

``` r
asterisks(
  p,
  trends = TRUE,
  levels = c(0.1, 0.05, 0.01, 0.001),
  marks = c(sig = "*", trend = "+", ns = NA_character_),
  include_key = FALSE
)
```

## Arguments

- p:

  a numeric vector of p-values.

- trends:

  logical. Should trends (e.g., .05 \< p \< .10) be marked?

- levels:

  a numeric vector demarcating ranges of p values that should receive
  unique significance marks.

- marks:

  a named character vector specifying marks for significance, trend, and
  non-significance.

- include_key:

  logical. should a key for significance marks be included as an
  attribute?

## Value

A character vector of asterisks corresponding to the p-values. If
`include_key = TRUE`, the vector will have a 'key' attribute indicating
the significance levels.

## Examples

``` r
p <- c(0.5, 0.09, 0.03, 0.008, 0.0003)

tibble::tibble(p, sig = asterisks(p))
#> # A tibble: 5 × 2
#>        p sig  
#>    <dbl> <fct>
#> 1 0.5    NA   
#> 2 0.09   +    
#> 3 0.03   *    
#> 4 0.008  **   
#> 5 0.0003 ***  

tibble::tibble(p, sig = asterisks(p, trends = FALSE))
#> # A tibble: 5 × 2
#>        p sig  
#>    <dbl> <fct>
#> 1 0.5    NA   
#> 2 0.09   NA   
#> 3 0.03   *    
#> 4 0.008  **   
#> 5 0.0003 ***  

tibble::tibble(p, sig = asterisks(p, trends = FALSE, marks = c(ns = "ns")))
#> # A tibble: 5 × 2
#>        p sig  
#>    <dbl> <fct>
#> 1 0.5    ns   
#> 2 0.09   ns   
#> 3 0.03   *    
#> 4 0.008  **   
#> 5 0.0003 ***  

asterisks(p, include_key = TRUE)
#> [1] <NA> +    *    **   *** 
#> attr(,"key")
#>   ***    **     *     + 
#> 0.001 0.010 0.050 0.100 
#> Levels: *** ** * +
```

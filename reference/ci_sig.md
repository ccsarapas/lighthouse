# Test whether a confidence interval excludes a given value

Tests whether a confidence interval excldes a specified reference value.
This will generally be the null value for the relevant test, such that
excluding this value indicates the test is statistically significant.

## Usage

``` r
ci_sig(
  ll,
  ul,
  reference = 1,
  return = c("logical", "asterisks"),
  marks = c("*", NA_character_)
)
```

## Arguments

- ll:

  a numeric vector containing confidence interval lower limits.

- ul:

  a numeric vector containing corresponding upper limits.

- reference:

  the value to check against. This will generally be the null value for
  the relevant test (e.g., 1 for odds ratios, 0 for beta coefficients).

- return:

  if `"logical"`, will return a logical vector indicating whether each
  confidence interval excludes `reference`. If `asterisks`, will return
  a character vector, using the characters passed to `marks`.

- marks:

  a length-2 vector specifying strings to mark significant and
  non-significant results when `return = "asterisks"`.

## Value

- If `return = \"logical\"` (default), a logical vector.

- If `return = \"asterisks\"`, a character vector.

## Examples

``` r
beta_CIs <- glm(
    Survived ~ Sex * Age,
    family = binomial,
    weights = Freq,
    data = as.data.frame(Titanic)
  ) %>%
  confint()
#> Waiting for profiling to be done...
OR_CIs <- tibble::as_tibble(exp(beta_CIs), rownames = "term")
beta_CIs <- tibble::as_tibble(beta_CIs, rownames = "term")

beta_CIs %>%
  dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`))
#> # A tibble: 4 × 4
#>   term               `2.5 %` `97.5 %` sig  
#>   <chr>                <dbl>    <dbl> <lgl>
#> 1 (Intercept)        -0.687     0.303 TRUE 
#> 2 SexFemale          -0.0834    1.48  FALSE
#> 3 AgeAdult           -1.69     -0.669 TRUE 
#> 4 SexFemale:AgeAdult  0.918     2.56  FALSE

beta_CIs %>%
  dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`, return = "asterisks"))
#> # A tibble: 4 × 4
#>   term               `2.5 %` `97.5 %` sig  
#>   <chr>                <dbl>    <dbl> <chr>
#> 1 (Intercept)        -0.687     0.303 *    
#> 2 SexFemale          -0.0834    1.48  NA   
#> 3 AgeAdult           -1.69     -0.669 *    
#> 4 SexFemale:AgeAdult  0.918     2.56  NA   

beta_CIs %>%
  dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`, return = "asterisks", marks = c("*", "ns")))
#> # A tibble: 4 × 4
#>   term               `2.5 %` `97.5 %` sig  
#>   <chr>                <dbl>    <dbl> <chr>
#> 1 (Intercept)        -0.687     0.303 *    
#> 2 SexFemale          -0.0834    1.48  ns   
#> 3 AgeAdult           -1.69     -0.669 *    
#> 4 SexFemale:AgeAdult  0.918     2.56  ns   

OR_CIs %>%
  dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`, reference = 1, return = "asterisks"))
#> # A tibble: 4 × 4
#>   term               `2.5 %` `97.5 %` sig  
#>   <chr>                <dbl>    <dbl> <chr>
#> 1 (Intercept)          0.503    1.35  NA   
#> 2 SexFemale            0.920    4.39  NA   
#> 3 AgeAdult             0.185    0.512 *    
#> 4 SexFemale:AgeAdult   2.50    12.9   *    
```

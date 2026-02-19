# Descending cumulative sum

Returns a cumulative sum beginning with the last element of `x`.

## Usage

``` r
cumsum_desc(x)
```

## Examples

``` r
ggplot2::diamonds %>%
  dplyr::count(cut) %>%
  dplyr::mutate(
    or_worse = cumsum(n),
    or_better = cumsum_desc(n)
  )
#> # A tibble: 5 × 4
#>   cut           n or_worse or_better
#>   <ord>     <int>    <int>     <int>
#> 1 Fair       1610     1610     53940
#> 2 Good       4906     6516     52330
#> 3 Very Good 12082    18598     47424
#> 4 Premium   13791    32389     35342
#> 5 Ideal     21551    53940     21551
```

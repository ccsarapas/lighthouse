# Find common prefixes or suffixes

Returns substring beginnings or endings common to all elements in
vector.

## Usage

``` r
str_prefix(string, na.rm = FALSE)

str_suffix(string, na.rm = FALSE)
```

## Examples

``` r
test_words <- c("antidote", "antimony", "antimatter", "antisense")
str_prefix(test_words)
#> [1] "anti"

wdays <- c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
)
str_suffix(wdays)
#> [1] "day"
```

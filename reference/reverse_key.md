# Reverse key a numeric vector

Reverses a numeric vector `x` by subtracting from `min` and adding
`max`. Observed minimum and maximum of `x` are used unless otherwise
specified.

## Usage

``` r
reverse_key(x, na.rm = FALSE, max = NULL, min = NULL)
```

## Examples

``` r
reverse_key(1:5)
#> [1] 5 4 3 2 1
reverse_key(3:5)
#> [1] 5 4 3
reverse_key(3:5, min = 1, max = 5)
#> [1] 3 2 1
```

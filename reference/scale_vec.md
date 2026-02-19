# Scaling and centering of vectors

A wrapper around [`base::scale()`](https://rdrr.io/r/base/scale.html)
that returns a vector instead of a matrix.

## Usage

``` r
scale_vec(x, center = TRUE, scale = TRUE)
```

## Examples

``` r
# using base::scale()
scale(0:4)
#>            [,1]
#> [1,] -1.2649111
#> [2,] -0.6324555
#> [3,]  0.0000000
#> [4,]  0.6324555
#> [5,]  1.2649111
#> attr(,"scaled:center")
#> [1] 2
#> attr(,"scaled:scale")
#> [1] 1.581139

# using scale_vec()
scale_vec(0:4)
#> [1] -1.2649111 -0.6324555  0.0000000  0.6324555  1.2649111
```

# Test if two objects have the same shape

Checks if two objects `x` and `y` have the same shape, i.e., the same
dimensions or same length for vectors.

## Usage

``` r
eq_shape(x, y)
```

## Arguments

- x:

  An object.

- y:

  An object.

## Value

`TRUE` if `x` and `y` have the same shape, `FALSE` otherwise.

## Examples

``` r
eq_shape(1:5, 1:5)
#> [1] TRUE
eq_shape(1:5, 1:6)
#> [1] FALSE
eq_shape(matrix(1:6, nrow = 2), matrix(1:6, nrow = 3))
#> [1] FALSE
eq_shape(matrix(1:6, nrow = 2), matrix(1:6, nrow = 2))
#> [1] TRUE
```

# Set comparison with automatic naming

Compares two sets (vectors), returning the elements unique to each and
the elements they have in common. Optionally names the returned sets
based on the supplied object names.

## Usage

``` r
set_compare(x, y, autoname = TRUE)
```

## Arguments

- x:

  A vector representing the first set.

- y:

  A vector representing the second set.

- autoname:

  Logical, whether to automatically name the returned sets based on the
  supplied object names. Defaults to `TRUE`.

## Value

A named list with three elements:

- Elements unique to `x`

- Elements unique to `y`

- Elements common to both `x` and `y`

## Examples

``` r
v1 <- c(1, 2, 3, 4)
v2 <- c(3, 4, 5, 6)

set_compare(v1, v2)
#> $v1
#> [1] 1 2
#> 
#> $v2
#> [1] 5 6
#> 
#> $intersect
#> [1] 3 4
#> 
set_compare(v1, v2, autoname = FALSE)
#> $x
#> [1] 1 2
#> 
#> $y
#> [1] 5 6
#> 
#> $intersect
#> [1] 3 4
#> 
```

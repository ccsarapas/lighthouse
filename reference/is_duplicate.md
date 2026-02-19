# Identify duplicates within a vector or vectors

This function checks for duplicated values within a vector or a set of
vectors.

## Usage

``` r
is_duplicate(..., nmax = 1, incomparables = FALSE)
```

## Arguments

- ...:

  one or more vectors of equal length.

- nmax:

  maximum number of times a value can appear before being considered a
  duplicate.

- incomparables:

  should missing values (including `NaN`) be considered duplicates?

## Value

a logical vector.

## Examples

``` r
x <- c(1, 2, 2, 3, 3, 3)
y <- c(1, 1, 2, 1, 2, 2)
is_duplicate(x)
#> [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
is_duplicate(x, nmax = 2)
#> [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE
is_duplicate(x, y)
#> [1] FALSE FALSE FALSE FALSE  TRUE  TRUE

z <- c(1, NA, NA)
is_duplicate(z)
#> [1] FALSE FALSE FALSE
is_duplicate(z, incomparables = TRUE)
#> [1] FALSE  TRUE  TRUE
```

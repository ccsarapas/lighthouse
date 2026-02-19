# Set NA values based on range of numbers.

Changes values in range `range_min` to `range_max` to `NA`. Works on
numeric vectors as well as for numbers in character vectors, factor
labels, or in numeric or character vectors with classes `labelled` or
`haven_labelled`.

## Usage

``` r
na_if_range(x, range_min = -Inf, range_max = -1)

# S3 method for class 'numeric'
na_if_range(x, range_min = -Inf, range_max = -1)

# S3 method for class 'character'
na_if_range(x, range_min = -Inf, range_max = -1)

# S3 method for class 'factor'
na_if_range(x, range_min = -Inf, range_max = -1)

# S3 method for class 'labelled'
na_if_range(x, range_min = -Inf, range_max = -1)

# S3 method for class 'haven_labelled'
na_if_range(x, range_min = -Inf, range_max = -1)
```

## Arguments

- x:

  A numeric vector, character vector, or factor.

- range_min:

  The minimum value to set to `NA`. Defaults to `-Inf`.

- range_max:

  The maximum value to set to `NA`. Defaults to `-1`.

## Note

Previously known as `coerce_na_range`, which is retained as an alias for
backward compatibility.

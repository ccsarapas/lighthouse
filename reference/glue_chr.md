# Format and interpolate a string as character vector

A wrapper around
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) which
returns a character vector rather than a `"glue"` object.

## Usage

``` r
glue_chr(...)
```

## Arguments

- ...:

  \[`expressions`\]  
  Unnamed arguments are taken to be expression string(s) to format.
  Multiple inputs are concatenated together before formatting. Named
  arguments are taken to be temporary variables available for
  substitution.

  For `glue_data()`, elements in `...` override the values in `.x`.

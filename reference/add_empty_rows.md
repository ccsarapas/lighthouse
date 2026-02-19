# Add empty rows

Adds the number of empty rows passed to `.nrows` (default 1) at
positions passed to `.before` or `.after`. Vectorized over `.before`,
`.after`, and `.nrows`.

## Usage

``` r
add_empty_rows(.data, .before = NULL, .after = NULL, .nrows = 1)
```

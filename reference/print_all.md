# Print all tibble rows

Actually limits printing to `RStudioPreference` `"console_max_lines"`
(or 1000 lines if not running in RStudio) unless otherwise specified in
`max`. Works only with `tibble`s, not
[`base::data.frame`](https://rdrr.io/r/base/data.frame.html)s.

## Usage

``` r
print_all(x, ..., max = NULL)
```

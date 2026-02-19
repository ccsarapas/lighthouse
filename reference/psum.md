# Parallel / row-wise sums and means

Given one or more vectors, `psum()` and `pmean()` return row-wise sums
or means. They are analogous to
[`base::pmin()`](https://rdrr.io/r/base/Extremes.html) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html). Like
[`pmin()`](https://rdrr.io/r/base/Extremes.html) /
[`pmax()`](https://rdrr.io/r/base/Extremes.html), these functions:

- accept multiple vectors via `...` (as opposed to
  [`rowSums()`](https://rdrr.io/r/base/colSums.html) and
  [`rowMeans()`](https://rdrr.io/r/base/colSums.html), which accept a
  single array). This allows them to be used in data-masking contexts
  (e.g., inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)).

- return `NA` when all values are `NA` and `na.rm = TRUE`. (as opposed
  to [`rowSums()`](https://rdrr.io/r/base/colSums.html), which returns
  `0` in this situation).

## Usage

``` r
psum(..., na.rm = FALSE)

pmean(..., na.rm = FALSE)
```

## Arguments

- ...:

  one or more numeric or logical vectors of equal length or length 1.

- na.rm:

  logical. Should missing values (including `NaN`) be removed?

## Value

A numeric vector.

## Details

Both `psum()` and
[`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md)
can be used in data-masking contexts, but have different capabilities
and limitations. See the "Details" section of
[`?psum_across`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md).

Note that, unlike [`pmin()`](https://rdrr.io/r/base/Extremes.html),
[`pmax()`](https://rdrr.io/r/base/Extremes.html),
[`rowSums()`](https://rdrr.io/r/base/colSums.html), and
[`rowMeans()`](https://rdrr.io/r/base/colSums.html), these functions do
not currently support objects with more than one dimension (e.g.,
matrices, arrays, or data frames).

## See also

- [`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md)
  for variants that support tidyselect expressions. See the "Details"
  section for relative strengths of `psum()` / `pmean()` vs.
  [`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md)
  /
  [`pmean_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md).

- [`base::pmin()`](https://rdrr.io/r/base/Extremes.html) for analogous
  [`pmin()`](https://rdrr.io/r/base/Extremes.html) and
  [`pmax()`](https://rdrr.io/r/base/Extremes.html) functions

- [`sum_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md)
  for non-parallel sums with similar `NA` handling

## Examples

``` r
psum(1:5, 6:10)
#> [1]  7  9 11 13 15
pmean(1:5, 6:10)
#> [1] 3.5 4.5 5.5 6.5 7.5

dat <- tibble::tribble(
  ~product,    ~price1, ~price2, ~price3,
  "Product 1", 20,      25,      22,
  "Product 2", NA,      30,      29,
  "Product 3", 15,      NA,      NA,
  "Product 4", NA,      NA,      NA
)

# contrast w `rowSums()` / `rowMeans()`:
# no need for `pick()` and different `NA` behavior
dat %>%
  dplyr::mutate(
    rowSums = rowSums(pick(price1, price2, price3), na.rm = TRUE),
    psum = psum(price1, price2, price3, na.rm = TRUE),
    rowMeans = rowMeans(pick(price1, price2, price3), na.rm = TRUE),
    pmean = pmean(price1, price2, price3, na.rm = TRUE)
  )
#> # A tibble: 4 × 8
#>   product   price1 price2 price3 rowSums  psum rowMeans pmean
#>   <chr>      <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl> <dbl>
#> 1 Product 1     20     25     22      67    67     22.3  22.3
#> 2 Product 2     NA     30     29      59    59     29.5  29.5
#> 3 Product 3     15     NA     NA      15    15     15    15  
#> 4 Product 4     NA     NA     NA       0    NA    NaN    NA  
```

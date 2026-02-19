# tidyselect-friendly row-wise aggregation

Wrappers around [`base::pmin()`](https://rdrr.io/r/base/Extremes.html),
[`base::pmax()`](https://rdrr.io/r/base/Extremes.html),
[`lighthouse::psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md),
and
[`lighthouse::pmean()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
that accept
[`tidyselect expressions`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html).

## Usage

``` r
psum_across(..., na.rm = FALSE)

pmean_across(..., na.rm = FALSE)

pmin_across(..., na.rm = FALSE)

pmax_across(..., na.rm = FALSE)
```

## Arguments

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  one or more tidyselect expressions that capture numeric and/or logical
  columns.

- na.rm:

  Should missing values (including `NaN`) be removed?

## Details

Lighthouse includes two sets of functions for computing "parallel" or
row-wise aggregates:

- [`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
  and
  [`pmean()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
  (which complement
  [`base::pmin()`](https://rdrr.io/r/base/Extremes.html) and
  [`pmax()`](https://rdrr.io/r/base/Extremes.html))

- `pmin_across()`, `pmax_across()`, `psum_across()`, and
  `pmean_across()`

Both sets of functions differ from
[`base::rowSums()`](https://rdrr.io/r/base/colSums.html) and
[`rowMeans()`](https://rdrr.io/r/base/colSums.html) in that they:

- work in data-masking contexts (e.g., inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html))
  without needing helpers like
  [`dplyr::pick()`](https://dplyr.tidyverse.org/reference/pick.html) or
  [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html).

- accept multiple inputs via `...`.

- return `NA` when `na.rm = TRUE` and all values in a row are `NA`. This
  mirrors behavior of
  [`base::pmin()`](https://rdrr.io/r/base/Extremes.html) and
  [`pmax()`](https://rdrr.io/r/base/Extremes.html), but differs from
  [`rowSums()`](https://rdrr.io/r/base/colSums.html), which returns `0`
  in this situation.

`psum_across()` and friends support tidyselect expressions; e.g.,

    dat 
      mutate(
        IDScrTotal = psum_across(IDScr1:IDScr6),
        SDScrTotal = psum_across(starts_with("SDScr"))
      )

...but must be used inside a data-masking verb like
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html), or
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html), and do
not support implicit computations.

Conversely,
[`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md) and
friends do not support tidyselect expressions, but can be used both
inside or outside a data-masking context:

    # data-masking
    dat 
      mutate(
        NumColors = psum(Red, Blue, Green),
      )

    #non-data masking
    psum(1:10, 6:15, 11:20)

and support "on the fly" or "implicit" computations:

    dat 
      mutate(
        CriteriaMet = psum(a1 > a2, pmax(b1, b2, b3) > 5, c1 < 10),
        TotalProp = psum(num1 / denom1, num2 / denom2, num3 / denom3)
      )

## See also

[`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html) for non-tidyselect
parallel aggregates.

## Examples

``` r
dat <- tibble::tribble(
  ~product,    ~price1, ~price2, ~price3,
  "Product 1", 20,      25,      22,
  "Product 2", NA,      30,      29,
  "Product 3", 15,      NA,      NA,
  "Product 4", NA,      NA,      NA
)

price_cols <- c("price1", "price2", "price3")

dat %>%
  dplyr::mutate(
    min = pmin_across(price1, price2, price3, na.rm = TRUE),
    max = pmax_across(price1:price3, na.rm = TRUE),
    sum = psum_across(starts_with("price"), na.rm = TRUE),
    mean = pmean_across(all_of(price_cols), na.rm = TRUE)
  )
#> # A tibble: 4 × 8
#>   product   price1 price2 price3   min   max   sum  mean
#>   <chr>      <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Product 1     20     25     22    20    25    67  22.3
#> 2 Product 2     NA     30     29    29    30    59  29.5
#> 3 Product 3     15     NA     NA    15    15    15  15  
#> 4 Product 4     NA     NA     NA    NA    NA    NA  NA  
```

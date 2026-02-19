# Custom summary table

Generates a summary table, with a row for each variable passed to
`.vars` and a column for each function passed to `...`.

## Usage

``` r
summary_table(
  .data,
  ...,
  .vars = where(is.numeric),
  na.rm = FALSE,
  .rows_group_by = NULL,
  .cols_group_by = NULL,
  .cols_group_glue = NULL,
  .cols_group_order = c("by_group", "by_function"),
  .var_col_name = "Variable",
  .cols_group_opts = list()
)
```

## Arguments

- .data:

  A data frame or data frame extension (e.g. a tibble).

- ...:

  Functions to apply to each variable specified in `.vars`, as function
  names (e.g., `mean`) or anonymous functions (e.g.,
  `\(x) mean(x, na.rm = TRUE)`). Each function passed to `...` will
  yield one column in the output table (or one column per group if
  `.cols_group_by` is specified). Output column names can optionally be
  specified. E.g., `m = mean, sd, sem = \(x) sd(x) / sqrt(n())` yields
  output columns named `m`, `sd`, and `sem`.

- .vars:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns in `.data` to summarize. Each column passed to `.vars` will
  yield one row in the output table (or one row per group if
  `.rows_group_by` is specified).

- na.rm:

  A logical value passed to any functions in `...` that take an `na.rm`
  argument.

- .rows_group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Grouping variable(s) for output rows.

- .cols_group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Grouping variable(s) for output columns.

- .cols_group_glue:

  A glue specification controlling column names when `.cols_group_by` is
  specified. See examples.

- .cols_group_order:

  When `.cols_group_by` is specified, how should the resulting columns
  be ordered?

  - "by_group" keeps columns from each group together, resulting in
    columns ordered like: fn1_group1, fn2_group1, fn1_group2,
    fn2_group2. This is the default.

  - "by_function" keeps columns from each function together, resulting
    in columns ordered like: fn1_group1, fn1_group2, fn2_group1,
    fn2_group2.

- .var_col_name:

  The name of the output column containing variable names passed to
  `.vars`. This column will be dropped if `.var_col_name` is `NULL` and
  only one variable is passed to `.vars`.

- .cols_group_opts:

  DEPRECATED. A list of additional arguments passed to
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  if `.cols_group_by` is specified. This argument will be removed in a
  future release.

## Examples

``` r
# example data
mtcars2 <- mtcars %>%
  dplyr::mutate(
    Transmission = dplyr::recode(am, `0` = "auto", `1` = "manual")
  )

# simple summary table. note specification of column and row names
# for "n", "m", and "weight".
mtcars2 %>%
  summary_table(
    n = n_valid, m = mean, sd,
    .vars = c(mpg, hp, weight = wt)
  )
#> # A tibble: 3 × 4
#>   Variable     n      m     sd
#>   <chr>    <int>  <dbl>  <dbl>
#> 1 mpg         32  20.1   6.03 
#> 2 hp          32 147.   68.6  
#> 3 weight      32   3.22  0.978

# with column and row groupings
mtcars2 %>%
  summary_table(
    n = n_valid, m = mean, sd,
    .vars = c(mpg, hp, weight = wt),
    .cols_group_by = cyl,
    .rows_group_by = Transmission
  )
#> # A tibble: 6 × 11
#>   Transmission Variable   n_4   m_4   sd_4   n_6    m_6   sd_6   n_8    m_8
#>   <chr>        <chr>    <int> <dbl>  <dbl> <int>  <dbl>  <dbl> <int>  <dbl>
#> 1 auto         mpg          3 22.9   1.45      4  19.1   1.63     12  15.0 
#> 2 auto         hp           3 84.7  19.7       4 115.    9.18     12 194.  
#> 3 auto         weight       3  2.94  0.408     4   3.39  0.116    12   4.10
#> 4 manual       mpg          8 28.1   4.48      3  20.6   0.751     2  15.4 
#> 5 manual       hp           8 81.9  22.7       3 132.   37.5       2 300.  
#> 6 manual       weight       8  2.04  0.409     3   2.76  0.128     2   3.37
#> # ℹ 1 more variable: sd_8 <dbl>

# `.var_col_name = NULL` will drop the variable name column if only one
# variable is included in `.vars`
mtcars2 %>%
 summary_table(
   n = n_valid, m = mean, sd,
   .vars = mpg,
   .cols_group_by = cyl,
   .rows_group_by = Transmission,
   .var_col_name = NULL
 )
#> # A tibble: 2 × 10
#>   Transmission   n_4   m_4  sd_4   n_6   m_6  sd_6   n_8   m_8  sd_8
#>   <chr>        <int> <dbl> <dbl> <int> <dbl> <dbl> <int> <dbl> <dbl>
#> 1 auto             3  22.9  1.45     4  19.1 1.63     12  15.0 2.77 
#> 2 manual           8  28.1  4.48     3  20.6 0.751     2  15.4 0.566

# customize column names with a glue specification:
mtcars2 %>%
  summary_table(
    M = mean, SD = sd,
    .vars = c(mpg, hp, weight = wt),
    .cols_group_by = c(Transmission, cyl),
    .cols_group_glue = "{cyl} cyl {Transmission}: {.value}"
  )
#> # A tibble: 3 × 13
#>   Variable `4 cyl auto: M` `4 cyl auto: SD` `6 cyl auto: M` `6 cyl auto: SD`
#>   <chr>              <dbl>            <dbl>           <dbl>            <dbl>
#> 1 mpg                22.9             1.45            19.1             1.63 
#> 2 hp                 84.7            19.7            115.              9.18 
#> 3 weight              2.94            0.408            3.39            0.116
#> # ℹ 8 more variables: `8 cyl auto: M` <dbl>, `8 cyl auto: SD` <dbl>,
#> #   `4 cyl manual: M` <dbl>, `4 cyl manual: SD` <dbl>, `6 cyl manual: M` <dbl>,
#> #   `6 cyl manual: SD` <dbl>, `8 cyl manual: M` <dbl>, `8 cyl manual: SD` <dbl>
```

# Alternative column ordering and naming for pivot_wider()

Note that as of 2024, some of `pivot_wider_alt()`'s functionality is now
supported in
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).
In particular, the functionality of `names_value_first = TRUE` in
`pivot_wider_alt()` can now be achieved by using
`names_vary = "slowest"` in `pivot_wider()`. The functionality of
`names_value_sep` can be achieved using the `names_glue` argument in
`pivot_wider()`.

A wrapper around
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
with additional options for sorting and naming of output columns, with
arguments `sort_by_col`, `names_value_first`, and `names_value_sep`
These options are only relevant when more than one input column is
passed to `values_from`.

## Usage

``` r
pivot_wider_alt(
  data,
  id_cols = NULL,
  names_from = name,
  sort_by_col = TRUE,
  names_value_first = TRUE,
  names_value_sep = ".",
  names_sep = "_",
  names_prefix = "",
  names_glue = NULL,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL
)
```

## Arguments

- data:

  A data frame to pivot.

- id_cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A set of columns that uniquely identify each observation. Typically
  used when you have redundant variables, i.e. variables whose values
  are perfectly correlated with existing variables.

  Defaults to all columns in `data` except for the columns specified
  through `names_from` and `values_from`. If a tidyselect expression is
  supplied, it will be evaluated on `data` after removing the columns
  specified through `names_from` and `values_from`.

- sort_by_col:

  If `TRUE` (default), output columns will be sorted by `names_from`,
  then `values_from`. (Differs from
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
  which sorts by `values_from` first, then `names_from`.)

- names_value_first:

  If `FALSE`, output columns will be named using a `{column}_{.value}`
  scheme. (Differs from
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
  which uses a `{.value}_{column}` scheme.)

- names_value_sep, names_sep:

  If `names_from` or `values_from` contain multiple variables, these
  will be used to join their values together into a single string to use
  as a column name. `names_value_sep` will separate `{.value}` from
  `{column}` components, while `names_sep` will separate `{column}`
  components from one another if `names_from` contains multiple
  variables. See Details and Examples.

- names_prefix, names_glue, names_repair, values_from, values_fill,
  values_fn:

  See documentation at
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

## Details

\#' The `names_value_sep` argument allows output column names that use a
different separator between `{.value}` and `{column}` than between
multiple `{columns}`s. Example:

    pivot_wider_alt(
      fakedata,
      names_from = c(size, color), # size = "sm", "med", "lg"; color = "red", "blue"
      values_from = c(n, weight),
      names_sep = "_",
      names_value_sep = ": "
    )

    # output column names:
    # `n: sm_red`, `weight: sm_red`, `n: sm_blue`, `weight: sm_blue`, `n: med_red`...

## Examples

``` r
data_ex <- ggplot2::diamonds %>%
  dplyr::group_by(cut, color) %>%
  dplyr::summarize(Min = min(price), Median = median(price), Max = max(price))
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by cut and color.
#> ℹ Output is grouped by cut.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(cut, color))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.

# default pivot_wider() behavior
data_ex %>%
  tidyr::pivot_wider(
    id_cols = color,
    names_from = cut,
    values_from = Min:Max
  )
#> # A tibble: 7 × 16
#>   color Min_Fair Min_Good `Min_Very Good` Min_Premium Min_Ideal Median_Fair
#>   <ord>    <int>    <int>           <int>       <int>     <int>       <dbl>
#> 1 D          536      361             357         367       367        3730
#> 2 E          337      327             352         326       326        2956
#> 3 F          496      357             357         342       408        3035
#> 4 G          369      394             354         382       361        3057
#> 5 H          659      368             337         368       357        3816
#> 6 I          735      351             336         334       348        3246
#> 7 J          416      335             336         363       340        3302
#> # ℹ 9 more variables: Median_Good <dbl>, `Median_Very Good` <dbl>,
#> #   Median_Premium <dbl>, Median_Ideal <dbl>, Max_Fair <int>, Max_Good <int>,
#> #   `Max_Very Good` <int>, Max_Premium <int>, Max_Ideal <int>

# pivot_wider_alt() behavior
data_ex %>%
  pivot_wider_alt(
    id_cols = color,
    names_from = cut,
    values_from = Min:Max
)
#> # A tibble: 7 × 16
#>   color Min.Fair Median.Fair Max.Fair Min.Good Median.Good Max.Good
#>   <ord>    <int>       <dbl>    <int>    <int>       <dbl>    <int>
#> 1 D          536        3730    16386      361       2728.    18468
#> 2 E          337        2956    15584      327       2420     18236
#> 3 F          496        3035    17995      357       2647     18686
#> 4 G          369        3057    18574      394       3340     18788
#> 5 H          659        3816    18565      368       3468.    18640
#> 6 I          735        3246    18242      351       3640.    18707
#> 7 J          416        3302    18531      335       3733     18325
#> # ℹ 9 more variables: `Min.Very Good` <int>, `Median.Very Good` <dbl>,
#> #   `Max.Very Good` <int>, Min.Premium <int>, Median.Premium <dbl>,
#> #   Max.Premium <int>, Min.Ideal <int>, Median.Ideal <dbl>, Max.Ideal <int>

# with `names_value_first` = FALSE
data_ex %>%
  pivot_wider_alt(
    id_cols = color,
    names_from = cut,
    values_from = Min:Max,
    names_value_first = FALSE
  )
#> # A tibble: 7 × 16
#>   color Fair.Min Fair.Median Fair.Max Good.Min Good.Median Good.Max
#>   <ord>    <int>       <dbl>    <int>    <int>       <dbl>    <int>
#> 1 D          536        3730    16386      361       2728.    18468
#> 2 E          337        2956    15584      327       2420     18236
#> 3 F          496        3035    17995      357       2647     18686
#> 4 G          369        3057    18574      394       3340     18788
#> 5 H          659        3816    18565      368       3468.    18640
#> 6 I          735        3246    18242      351       3640.    18707
#> 7 J          416        3302    18531      335       3733     18325
#> # ℹ 9 more variables: `Very Good.Min` <int>, `Very Good.Median` <dbl>,
#> #   `Very Good.Max` <int>, Premium.Min <int>, Premium.Median <dbl>,
#> #   Premium.Max <int>, Ideal.Min <int>, Ideal.Median <dbl>, Ideal.Max <int>

# multiple `names_from` vars, with different value vs. name separators
ggplot2::mpg %>%
  dplyr::filter(class %in% c("compact", "subcompact", "midsize")) %>%
  dplyr::group_by(
    manufacturer,
    trans = stringr::str_extract(trans, ".*(?=\\()"), year
  ) %>%
  dplyr::summarize(across(c(cty, hwy), mean)) %>%
  pivot_wider_alt(
    names_from = trans:year,
    values_from = cty:hwy,
    names_sep = "_",
    names_value_sep = ": "
  )
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by manufacturer, trans, and year.
#> ℹ Output is grouped by manufacturer and trans.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(manufacturer, trans, year))` for per-operation
#>   grouping (`?dplyr::dplyr_by`) instead.
#> # A tibble: 10 × 9
#> # Groups:   manufacturer [10]
#>    manufacturer `cty: auto_1999` `hwy: auto_1999` `cty: auto_2008`
#>    <chr>                   <dbl>            <dbl>            <dbl>
#>  1 audi                     16               25.8             18  
#>  2 chevrolet                18.5             26.5             19  
#>  3 ford                     16.5             23               15.5
#>  4 honda                    24               32               24.5
#>  5 hyundai                  18.3             26               19.2
#>  6 nissan                   18.5             26.5             20.3
#>  7 pontiac                  17               26.3             17  
#>  8 subaru                   20               26               20  
#>  9 toyota                   21               28.2             21.2
#> 10 volkswagen               19.4             28.1             20.2
#> # ℹ 5 more variables: `hwy: auto_2008` <dbl>, `cty: manual_1999` <dbl>,
#> #   `hwy: manual_1999` <dbl>, `cty: manual_2008` <dbl>,
#> #   `hwy: manual_2008` <dbl>
```

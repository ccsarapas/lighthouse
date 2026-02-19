# Split dataframe by named groups

Divides `.data` into a named list of dataframes defined by the grouping
structure. Grouping variables can optionally be passed to `...`. A
nested list will be returned if there is more than one grouping variable
and `.nested` = `TRUE`.

## Usage

``` r
group_split_named(
  .data,
  ...,
  .keep = TRUE,
  .sep = ".",
  .col_names = FALSE,
  .col_sep = "_",
  .nested = FALSE,
  .na.rm = FALSE,
  .add_groups = TRUE
)
```

## Examples

``` r
by_cyl_gear1 <- mtcars %>%
  group_split_named(cyl, gear, .col_names = TRUE)
by_cyl_gear1$cyl_6.gear_4
#> # A tibble: 4 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#> 3  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> 4  17.8     6  168.   123  3.92  3.44  18.9     1     0     4     4

by_cyl_gear2 <- mtcars %>%
  group_split_named(cyl, gear, .col_names = TRUE, .nested = TRUE)
by_cyl_gear2$cyl_6
#> $gear_3
#> # A tibble: 2 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> 2  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1
#> 
#> $gear_4
#> # A tibble: 4 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#> 3  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> 4  17.8     6  168.   123  3.92  3.44  18.9     1     0     4     4
#> 
#> $gear_5
#> # A tibble: 1 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  19.7     6   145   175  3.62  2.77  15.5     0     1     5     6
#> 
by_cyl_gear2$cyl_6$gear_4
#> # A tibble: 4 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#> 3  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> 4  17.8     6  168.   123  3.92  3.44  18.9     1     0     4     4
```

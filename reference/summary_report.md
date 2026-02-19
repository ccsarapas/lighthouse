# Summarize variables based on measurement level

Summarizes each variable passed to `...`. This is handled differently
based on each variable's level of measurement:

- For nominal variables, returns *n* and proportion for each level

- For binary variables, returns *n* and proportion `TRUE`

- For continuous variables, returns mean and standard deviation by
  default. Specify alternative summary statistics using `.cont_fx`.

By default, `summary_report()` will guess the measurement level for each
variable. This can be overridden for all variables using the `.default`
argument, or for select variables using the `nom()`, `bin()`, or
`cont()` measurement wrappers. See details.

## Usage

``` r
summary_report(
  .data,
  ...,
  .default = c("auto", "nom", "bin", "cont"),
  .drop = TRUE,
  .cont_fx = list(mean, sd),
  .missing_label = NA,
  na.rm = FALSE,
  na.rm.nom = na.rm,
  na.rm.bin = na.rm,
  na.rm.cont = na.rm
)

nom(...)

bin(...)

cont(...)
```

## Arguments

- .data:

  a data frame or data frame extension.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  one or more variable names. and/or tidyselect expressions. Elements
  may be wrapped in `nom()`, `bin()`, or `cont()` to force summarizing
  as binary, nominal, or continuous, respectively; see details.

- .default:

  how to determine measurement level for variables if not specified by a
  measurement wrapper. `"auto"` will guess measurement level for each
  variable, while `"nom"`, `"bin"`, and `"cont"` will treat all
  unwrapped variables as nominal, binary, or continuous, respectively.

- .drop:

  if `FALSE`, frequencies for nominal variables will include counts for
  empty groups (i.e. for levels of factors that don't exist in the
  data).

- .cont_fx:

  a list containing the two functions with which continuous variables
  will be summarized.

- .missing_label:

  label for missing values in nominal variables.

- na.rm:

  if `TRUE`, `NA` values in each variable will be dropped prior to
  computation.

- na.rm.nom, na.rm.bin, na.rm.cont:

  control `NA` handling specifically for nominal, binary, or continuous
  variables. Overrides `na.rm` for that variable type.

## Value

A tibble with four columns:

- `Variable`: Variable name

- `Value`:

  - For nominal variables, a row for each unique value (including
    unobserved factor levels if `.drop = FALSE`).

  - For binary variables, either `TRUE` or `1` (for logical or numeric
    variables, respectively).

  - For continuous variables, the names of the summary statistics
    specified in `.cont_fx`.

- `V1`:

  - For nominal and binary variables, the number of observations with
    the value in `Value`.

  - For continuous variables, the value of the first summary statistic.

- `V2`:

  - For nominal and binary variables, the proportion of observations
    with the value in `Value`.

  - For continuous variables, the value of the second summary statistic.

## Determining measurement level

The measurement level for each variable is determined as follows:

1.  Variables wrapped in `nom()`, `bin()`, or `cont()` will be treated
    as nominal, binary, or continuous, respectively.

2.  Variables without a measurement wrapper will be treated as the type
    specified in `.default`.

3.  If `.default` is `"auto"`, measurement level will be inferred:

    - Logical vectors will be treated as binary if there are no missing
      values or if `na.rm.bin = TRUE`.

    - Character vectors, factors, dates and datetimes, and logical
      vectors with missing values will be treated as nominal.

    - All other variables will be treated as continuous.

## Support for binary variables

To be treated as binary, both of these must be true:

1.  The variable must be either a logical vector, or a binary numeric
    vector containing only 0s and 1s.

2.  The variable must not include any missing values, or `na.rm.bin`
    must be set to `TRUE`.

Future extensions may allow handling of other dichotomous variables
(e.g., `"Pregnant"` vs. `"Not pregnant"`), but this is not currently
supported. Instead, consider converting these to a logical indicator,
e.g., `Pregnant = PregnancyStatus == "Pregnant"`.

## Examples

``` r
mtcars %>% 
  transform(high_hp = hp > 200) %>%  # create logical indicator
  summary_report(
    nom(cyl),  # numeric - would autotype as continuous, but override with `nom()`
    bin(am),   # numeric - would autotype as continuous, but override with `bin()`
    high_hp,   # logical - will autotype as binary
    mpg,       # numeric - will autotype as continuous
    .cont_fx = list(median, IQR)
  )
#> # A tibble: 6 × 4
#>   Variable Value          V1    V2
#>   <chr>    <chr>       <dbl> <dbl>
#> 1 cyl      4            11   0.344
#> 2 cyl      6             7   0.219
#> 3 cyl      8            14   0.438
#> 4 am       1            13   0.406
#> 5 high_hp  TRUE          7   0.219
#> 6 mpg      median, IQR  19.2 7.38 

ggplot2::msleep %>%
  transform(herbivore = vore == "herbi") %>%
  summary_report(
    conservation, # character - will be autotyped as nominal
    herbivore, # logical, but w/ NAs - so will autotype as nominal
    sleep_total, # numeric - will be autotyped as continuous
    sleep_rem, # ditto, but has NAs so results will be NA
    .missing_label = "(missing)"
  )
#> # A tibble: 12 × 4
#>    Variable     Value           V1      V2
#>    <chr>        <chr>        <dbl>   <dbl>
#>  1 conservation cd             2    0.0241
#>  2 conservation domesticated  10    0.120 
#>  3 conservation en             4    0.0482
#>  4 conservation lc            27    0.325 
#>  5 conservation nt             4    0.0482
#>  6 conservation vu             7    0.0843
#>  7 conservation (missing)     29    0.349 
#>  8 herbivore    FALSE         44    0.530 
#>  9 herbivore    TRUE          32    0.386 
#> 10 herbivore    (missing)      7    0.0843
#> 11 sleep_total  mean, sd      10.4  4.45  
#> 12 sleep_rem    mean, sd      NA   NA     

# repeat above, but tweak NA handling
ggplot2::msleep %>%
  transform(herbivore = vore == "herbi") %>%
    summary_report(
      conservation, # character - will be autotyped as nominal
      herbivore,    # logical, w/ NAs - but will autotype as binary thanks to `na.rm.bin = TRUE`
      sleep_total,  # numeric - will be autotyped as continuous
      sleep_rem,    # numeric w/ NAs - but will still give results thanks to `na.rm.cont = TRUE`,
      na.rm.bin = TRUE,
      na.rm.cont = TRUE,
      .missing_label = "(missing)"
    )
#> # A tibble: 10 × 4
#>    Variable     Value           V1     V2
#>    <chr>        <chr>        <dbl>  <dbl>
#>  1 conservation cd            2    0.0241
#>  2 conservation domesticated 10    0.120 
#>  3 conservation en            4    0.0482
#>  4 conservation lc           27    0.325 
#>  5 conservation nt            4    0.0482
#>  6 conservation vu            7    0.0843
#>  7 conservation (missing)    29    0.349 
#>  8 herbivore    TRUE         32    0.421 
#>  9 sleep_total  mean, sd     10.4  4.45  
#> 10 sleep_rem    mean, sd      1.88 1.30  

if (FALSE) { # \dontrun{
# create a report using pre-processed SOR data
total_label <- "SOR-II Overall"
data_baseline %>%
  group_with_total(ServiceType, .label = total_label) %>%
  summary_report(
    Age, Gender, Race,
    bin(DAUseAlcohol, DAUseIllegDrugs, DAUseBoth),
    DAUseAlcoholDays, DAUseIllegDrugsDays, DAUseBothDays,
    DAUseAlcoholDaysOrdinal, DAUseIllegDrugsDaysOrdinal, DAUseBothDaysOrdinal,
    na.rm = TRUE,
    .drop = FALSE
  ) %>%
  pivot_wider(
    names_from = ServiceType,
    names_vary = "slowest",
    values_from = V1:V2
  ) %>%
  relocate(contains(total_label), .after = Value) %>%
  add_rows_at_value(Variable, Race, DAUseBoth, DAUseBothDays) %>%
  print_all()
} # }
```

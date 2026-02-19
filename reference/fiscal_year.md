# Get the fiscal year and quarter of a date-time

These functions return the fiscal year or fiscal year and quarter for a
date-time.

## Usage

``` r
fiscal_year(x, type = c("year", "date_first", "date_last"), fiscal_start = 1)

ffy(x, type = c("year", "date_first", "date_last"))

sfy_il(x, type = c("year", "date_first", "date_last"))

ffyq(x, type = c("year.quarter", "quarter", "date_first", "date_last"))

sfyq_il(x, type = c("year.quarter", "quarter", "date_first", "date_last"))
```

## Arguments

- x:

  A date or date-time vector.

- type:

  What to return:

  - For `fiscal_year()`, `ffy()`, and `sfy_il()`: One of "year"
    (default - numeric fiscal year), or "date_first" or "date_last"
    (first or last date of fiscal year).

  - For `ffyq()` and `sfyq_il()`: One of "year.quarter" (default -
    numeric fiscal year and quarter, in YYYY.Q format), "quarter"
    (numeric quarter), or "date_first" or "date_last" (first or last
    date of fiscal quarter).

- fiscal_start:

  For `fiscal_year`, the month the fiscal year starts (default is 1 for
  January).

## Value

numeric (if `type` is `"year"` or `"year.quarter"`) or a vector of class
Date (if `type` is `"date_first"` or `"date_last"`).

## Details

`ffy()` and `sfy_il()` are convenience wrappers around `fiscal_year()`
for federal fiscal year (starts in October) and Illinois state fiscal
year (starts in July).

`ffyq()` and `sfyq_il()` are analogous convenience wrappers around
[`lubridate::quarter()`](https://lubridate.tidyverse.org/reference/quarter.html).

## Examples

``` r
dates <- as.Date(c("2020-01-15", "2020-04-15", "2020-07-15", "2020-10-15"))

# default outputs
data.frame(
  date = dates,
  fiscal_year = fiscal_year(dates),
  fiscal_start_apr = fiscal_year(dates, fiscal_start = 4),
  sfy_il = sfy_il(dates),
  ffy = ffy(dates),
  sfyq_il = sfyq_il(dates),
  ffyq = ffyq(dates)
)
#>         date fiscal_year fiscal_start_apr sfy_il  ffy sfyq_il   ffyq
#> 1 2020-01-15        2020             2020   2020 2020  2020.3 2020.2
#> 2 2020-04-15        2020             2021   2020 2020  2020.4 2020.3
#> 3 2020-07-15        2020             2021   2021 2020  2021.1 2020.4
#> 4 2020-10-15        2020             2021   2021 2021  2021.2 2021.1

# with `type = "date_first"`
data.frame(
  date = dates,
  fiscal_year = fiscal_year(dates, type = "date_first"),
  fiscal_start_apr = fiscal_year(dates, type = "date_first", fiscal_start = 4),
  sfy_il = sfy_il(dates, type = "date_first"),
  ffy = ffy(dates, type = "date_first")
) 
#>         date fiscal_year fiscal_start_apr     sfy_il        ffy
#> 1 2020-01-15  2020-01-01       2019-04-01 2019-07-01 2019-10-01
#> 2 2020-04-15  2020-01-01       2020-04-01 2019-07-01 2019-10-01
#> 3 2020-07-15  2020-01-01       2020-04-01 2020-07-01 2019-10-01
#> 4 2020-10-15  2020-01-01       2020-04-01 2020-07-01 2020-10-01
```

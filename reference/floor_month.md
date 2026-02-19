# Floor methods for date objects

`floor_month()` and `floor_week()` are simple wrappers around
[`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
that round down to the first day of the month or week.

`floor_days()` rounds down to the nearest `n`-day increment. Floors are
defined relative to the earliest date in `x`, unless a different start
date is passed to `start`. Default behavior differs from
`lubridate::floor_date(x, unit = "{n} days")`, which "resets" the floor
to the first of the month with each month. lubridate-like behavior can
be achieved by setting `reset_monthly = TRUE`.

## Usage

``` r
floor_month(x)

floor_week(x, week_start = getOption("lubridate.week.start", 7))

floor_days(x, n = 1L, start = min(x, na.rm = TRUE), reset_monthly = FALSE)
```

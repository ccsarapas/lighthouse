# Format date-time to string without leading zeros

This wrapper around
[`base::strftime()`](https://rdrr.io/r/base/strptime.html) converts a
date or date-time object to character without leading zeros in specified
components.

## Usage

``` r
strftime_no_lead(
  x,
  format = "",
  tz = "",
  usetz = FALSE,
  no_lead = c("%d", "%m", "%H", "%I", "%F", "%r", "%R", "%T"),
  ...
)
```

## Arguments

- x:

  a Date, date-time,
  [`hms::hms`](https://hms.tidyverse.org/reference/hms.html) object, or
  other object coercible to `"POSIXlt"`.

- format:

  a character string. If `""` (the default), `"%Y-%m-%d %H:%M:%S"` will
  be used if any element has a time component which is not midnight, and
  `"%Y-%m-%d"` otherwise.

- tz:

  a character string specifying the time zone to be used for the
  conversion. System-specific (see
  [`as.POSIXlt`](https://rdrr.io/r/base/as.POSIXlt.html)), but `""` is
  the current time zone, and `"GMT"` is UTC. Invalid values are most
  commonly treated as UTC, on some platforms with a warning.

- usetz:

  logical. Should the time zone abbreviation be appended to the output?
  This is used in printing times, and more reliable than using `"%Z"`.

- no_lead:

  a character vector of POSIX conversion specifications from which
  leading 0s should be removed.

- ...:

  arguments passed to [`format()`](https://rdrr.io/r/base/format.html).

## Value

A character vector representing the date or date-time.

## See also

[`base::strftime()`](https://rdrr.io/r/base/strptime.html)

## Examples

``` r
dt <- as.POSIXct("2023-06-05 01:02:03")

# `base::strftime()` includes leading zeros
strftime(dt, "%m/%d/%y %H:%M:%S")
#> [1] "06/05/23 01:02:03"
# "06/05/23 01:02:03"

# `strftime_no_lead()` removes leading zeros from specific components
## by default, leading zeros removed from month, day, and hour
strftime_no_lead(dt, "%m/%d/%y %H:%M:%S")
#> [1] "6/5/23 1:02:03"
# "6/5/23 1:02:03"

## or remove from specified components -- eg, hour, minute, and second
strftime_no_lead(
  dt, 
  format = "%Hh %Mm %OS1s", 
  no_lead = c("%H", "%M", "%OS1")
)
#> [1] "1h 2m 3.0s"
# "1h 2m 3.0s"
```

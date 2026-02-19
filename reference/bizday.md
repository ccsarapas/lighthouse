# Find the nth or next business day

`nth_bizday()` returns the nth business day from a given date, based on
CHS, Illinois, or federal holidays. `next_bizday()` is a wrapper for

## Usage

``` r
nth_bizday(
  x,
  n,
  include_today = FALSE,
  holidays = c("Chestnut", "Illinois", "federal")
)

next_bizday(
  x,
  include_today = FALSE,
  holidays = c("Chestnut", "Illinois", "federal")
)
```

## Arguments

- x:

  a date or vector of dates.

- n:

  integer indicating ow many business days forward to find.

- include_today:

  logical indicating whether `x` be counted as one day (assuming it's a
  business day)?

- holidays:

  character indicating which set of holidays to use.

## Value

- `nth_bizday` returns the nth business day from the provided date(s).

- `next_bizday` returns the next business day from the provided date(s).

## Examples

``` r
next_bizday(as.Date("2024-07-02"))
#> [1] "2024-07-03"
nth_bizday(as.Date("2024-07-02"), 5)
#> [1] "2024-07-10"
nth_bizday(as.Date("2024-07-02"), 5, include_today = TRUE)
#> [1] "2024-07-09"
```

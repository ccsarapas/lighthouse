#' Convert all datetimes in table to dates
#'
#' Returns a dataframe with all datetime columns (i.e., class `POSIXct` or
#' `POSIXlt`) converted to `Date`.
#'
#' @export
datetimes_to_date <- function(.data) {
  .data %>%
    dplyr::mutate(
      dplyr::across(where(lubridate::is.POSIXt), lubridate::as_date)
    )
}



#' Find the nth or next business day
#'
#' `nth_bizday()` returns the nth business day from a given date, based on CHS, Illinois, or federal holidays. `next_bizday()` is a wrapper for
#'
#' @param x a date or vector of dates.
#' @param n integer indicating ow many business days forward to find.
#' @param include_today logical indicating whether `x` be counted as one day (assuming it's a business day)?
#' @param holidays character indicating which set of holidays to use.
#'
#' @return
#' - `nth_bizday` returns the nth business day from the provided date(s).
#' - `next_bizday` returns the next business day from the provided date(s).
#'
#' @examples
#' next_bizday(as.Date("2024-07-02"))
#' nth_bizday(as.Date("2024-07-02"), 5)
#' nth_bizday(as.Date("2024-07-02"), 5, include_today = TRUE)
#'
#' @name bizday
#' @export
nth_bizday <- function(x,
                       n,
                       include_today = FALSE,
                       holidays = c("Chestnut", "Illinois", "federal")) {
  bizdays <- business_days[[match.arg(holidays)]]
  inrange <- dplyr::between(x, attr(bizdays, "start"), attr(bizdays,
                                                            "end"))
  if (!rlang::is_scalar_integerish(n)) {
    stop("`n` must be a whole number of length 1.")
  }
  if (!all(is_TRUE_or_NA(inrange)))
    stop("Dates before ", attr(bizdays, "start"), " or after ",
         attr(bizdays, "end"), " are not currently supported.")
  if (!include_today)
    x <- x + lubridate::days(1)
  x <- purrr::map(lubridate::as_date(x), \(x) bizdays[bizdays >= x][[n]])
  do.call(c, x)
}
#' @rdname bizday
#' @export
next_bizday <- function(x,
                        include_today = FALSE,
                        holidays = c("Chestnut", "Illinois", "federal")) {
  nth_bizday(x = x, n = 1, include_today = include_today, holidays = holidays)
}


#' Floor methods for date objects
#'
#' @description
#' `floor_month()` and `floor_week()` are simple wrappers around
#' `lubridate::floor_date()` that round down to the first day of the month or
#' week.
#'
#' `floor_days()` rounds down to the nearest `n`-day increment. Floors are
#' defined relative to the earliest date in `x`, unless a different start date
#' is passed to `start`. Default behavior differs from
#' `lubridate::floor_date(x, unit = "{n} days")`, which "resets" the floor to
#' the first of the month with each month. `lubridate`-like behavior can be
#' achieved by setting `reset_monthly = TRUE`.
#'
#' @export
floor_month <- function(x) lubridate::floor_date(x, unit = "month")

#' @rdname floor_month
#'
#' @export
floor_week <- function(x, week_start = getOption("lubridate.week.start", 7)) {
  lubridate::floor_date(x, unit = "week", week_start = week_start)
}

#' @rdname floor_month
#'
#' @export
floor_days <- function(x,
                       n = 1L,
                       start = min(x, na.rm = TRUE),
                       reset_monthly = FALSE) {
  if (reset_monthly) {
    if (!missing(start))
      warning("`start` is ignored when `reset_monthly` = TRUE")
    return(lubridate::floor_date(x, unit = paste(n, "days")))
  }
  start <- lubridate::as_date(start)
  days_since = as.numeric(lubridate::as_date(x) - start, units = "days")
  days_since_floored <- (days_since %/% n) * n
  start + days_since_floored
}

#' Number of days between two dates
#'
#' Returns number of days between two dates.
#' @export
days_diff <- function(d1, d2, warn = TRUE) {
  if (xor(lubridate::is.POSIXt(d1), lubridate::is.POSIXt(d2))) {
    if (warn) warning("Datetime converted to Date to compute days difference")
    if (lubridate::is.POSIXct(d1)) d1 <- as.Date(d1)
    else d2 <- as.Date(d2)
  }
  as.numeric(d2 - d1, unit = "days")
}

#' Get the fiscal year and quarter of a date-time
#'
#' @description
#' These functions return the fiscal year or fiscal year and quarter for a date-time.
#'
#' @param x A date or date-time vector.
#' @param type What to return:
#' - For `fiscal_year()`, `ffy()`, and `sfy_il()`: One of "year" (default - numeric fiscal year), or "date_first" or "date_last" (first or last date of fiscal year).
#' - For `ffyq()` and `sfyq_il()`: One of "year.quarter" (default - numeric fiscal year and quarter, in YYYY.Q format), "quarter" (numeric quarter), or "date_first" or "date_last" (first or last date of fiscal quarter).
#' @param fiscal_start For `fiscal_year`, the month the fiscal year starts (default is 1 for January).
#'
#' @details
#' `ffy()` and `sfy_il()` are convenience wrappers around `fiscal_year()` for
#' federal fiscal year (starts in October) and Illinois state fiscal year
#' (starts in July).
#'
#' `ffyq()` and `sfyq_il()` are analogous convenience wrappers
#' around `lubridate::quarter()`.
#'
#' @return numeric (if `type` is `"year"` or `"year.quarter"`) or a vector of class Date (if `type` is `"date_first"` or `"date_last"`).
#'
#' @examples
#' dates <- as.Date(c("2020-01-15", "2020-04-15", "2020-07-15", "2020-10-15"))
#' 
#' # default outputs
#' data.frame(
#'   date = dates,
#'   fiscal_year = fiscal_year(dates),
#'   fiscal_start_apr = fiscal_year(dates, fiscal_start = 4),
#'   sfy_il = sfy_il(dates),
#'   ffy = ffy(dates),
#'   sfyq_il = sfyq_il(dates),
#'   ffyq = ffyq(dates)
#' )
#' 
#' # with `type = "date_first"`
#' data.frame(
#'   date = dates,
#'   fiscal_year = fiscal_year(dates, type = "date_first"),
#'   fiscal_start_apr = fiscal_year(dates, type = "date_first", fiscal_start = 4),
#'   sfy_il = sfy_il(dates, type = "date_first"),
#'   ffy = ffy(dates, type = "date_first")
#' ) 
#' 
#' @export
fiscal_year <- function(x,
                        type = c("year", "date_first", "date_last"),
                        fiscal_start = 1) {
  type <- match.arg(type)
  if (!(length(fiscal_start) == 1 &&
        is_coercible_integer(fiscal_start) && 
        fiscal_start >= 1 && 
        fiscal_start <= 12)) {
    cli::cli_abort(c("!" = "`fiscal_start` must be a single integer between 1 and 12"))
  }
  if (fiscal_start == 1) {
    fyear <- lubridate::year(x)
  } else {
    fyear <- ifelse(
      lubridate::month(x) < fiscal_start, 
      lubridate::year(x), 
      lubridate::year(x) + 1
    )
  }
  if (type == "year") return(fyear)
  if (fiscal_start != 1) fyear <- fyear - 1
  dfirst <- lubridate::make_date(fyear, fiscal_start, 1)
  if (type == "date_first") {
    dfirst
  } else {
    dfirst + lubridate::years(1) - lubridate::days(1)
  }
}
#' @rdname fiscal_year
#' @export
ffy <- function(x, type = c("year", "date_first", "date_last")) {
  fiscal_year(x, type = type, fiscal_start = 10)
}
#' @rdname fiscal_year
#' @export
sfy_il <- function(x, type = c("year", "date_first", "date_last")) {
  fiscal_year(x, type = type, fiscal_start = 7)
}
#' @rdname fiscal_year
#' @export
ffyq <- function(x,
                 type = c("year.quarter", "quarter", "date_first", "date_last")) {
  type <- match.arg(type)
  lubridate::quarter(x, type = type, fiscal_start = 10)
}
#' @rdname fiscal_year
#' @export
sfyq_il <- function(x,
                    type = c("year.quarter", "quarter", "date_first", "date_last")) {
  type <- match.arg(type)
  lubridate::quarter(x, type = type, fiscal_start = 7)
}

#' Format date-time to string without leading zeros
#'
#' This wrapper around [`base::strftime()`] converts a date or date-time object
#' to character without leading zeros in specified components.
#'
#' @inheritParams base::strftime
#' @param x a Date, date-time, or other object coercible to `"POSIXlt"`.
#' @param format a character string. If `""` (the default),
#'   `"%Y-%m-%d %H:%M:%S"` will be used if any element has a time component
#'   which is not midnight, and `"%Y-%m-%d"` otherwise. If
#'   `options("digits.secs")` is set, up to the specified number of digits will
#'   be printed for seconds.
#' @param no_lead a character vector of POSIX conversion specifications from
#'   which leading 0s should be removed.
#'
#' @return A character vector representing the date or date-time.
#'
#' @seealso [`base::strftime()`]
#'
#' @examples
#' dt <- as.POSIXct("2023-06-05 01:02:03")
#'
#' # with leading zeros
#' strftime(dt, "%m/%d/%Y %H:%M:%S")
#'
#' # without leading zeros
#' strftime_no_lead(dt, "%m/%d/%Y %H:%M:%S")
#'
#' @export
strftime_no_lead <- function(x,
                             format = "",
                             tz = "",
                             usetz = FALSE,
                             no_lead = c(
                               "%d", "%m", "%H", "%I", "%F", "%r", "%R", "%T"
                             ),
                             ...) {
  if (length(format) > 1) {
    stop("`strftime_no_lead()` does not support `format` with length > 1.")
  }
  specs_all <- c(
    "%%", "%a", "%A", "%b", "%B", "%c", "%C", "%d", "%D", "%e", "%F", "%g",
    "%G", "%h", "%H", "%I", "%j", "%m", "%M", "%n", "%p", "%r", "%R", "%S",
    "%t", "%T", "%u", "%U", "%V", "%w", "%W", "%x", "%X", "%y", "%Y", "%z", "%Z"
  )
  no_lead_unrec <- setdiff(no_lead, specs_all)
  if (length(no_lead_unrec) > 0) {
    stop(
      "Unrecognized conversion specification(s) in `no_lead`:\n",
      no_lead_unrec
    )
  }
  if (any(no_lead %in% c("%c", "%D", "%x", "%X"))) {
    stop(
      '`strftime_no_lead()` does not support the locale-dependent conversion ',
      'specifications "%c", "%D", "%x", or "%X" in the `no_lead` argument.'
    )
  }
  elements <- stringr::str_extract_all(format, "%\\S")[[1]] |>
    unique() |>
    stats::setNames(nm = _) |>
    lapply(strftime, x = x, tz = tz, ...)
  if ("%F" %in% names(elements) && "%F" %in% no_lead) {
    elements[["%F"]] <- stringr::str_remove_all(elements[["%F"]], "(?<=-)0")
    if (any(stringr::str_starts(elements[["%F"]], "0"))) {
      warning(
        'Leading zeros removed for month and day but not year components of ',
        '"%F". To change this behavior, use "%Y-%m-%d" and adjust `no_lead` ',
        'argument.'
      )
    }
    no_lead <- setdiff(no_lead, "%F")
  }
  for (nl in intersect(no_lead, names(elements))) {
    elements[[nl]] <- stringr::str_remove(elements[[nl]], "^0+(?=[0-9])")
  }
  out <- format
  for (el in names(elements)) {
    out <- stringr::str_replace_all(out, stringr::fixed(el), elements[[el]])
  }
  if (usetz) {
    stringr::str_replace(
      strftime(x, format = "_", tz = tz, usetz = TRUE, ...),
      "_",
      out
    )
  } else {
    out
  }
}

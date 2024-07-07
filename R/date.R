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


#' Determine fiscal year from date
#'
#' @description
#' Given a date, returns the corresponding fiscal year, start date, or end date. The `fiscal_year` function allows specifying the fiscal year start month, while `ffy` and `sfy_il` are convenience wrappers:
#' - `ffy`: Federal fiscal year (starts in October)
#' - `sfy_il`: Illinois state fiscal year (starts in July)
#'
#' @param x A date or date-time object.
#' @param type What to return: the fiscal year ("year"), first day of the fiscal year ("date_first"), or last day of the fiscal year ("date_last").
#' @param fiscal_start For `fiscal_year`, the month the fiscal year starts (default is 1 for January).
#'
#' @return An integer representing the fiscal year or a Date representing the start or end of the fiscal year, depending on `type`.
#'
#' @examples
#' dt <- as.Date("2023-08-15")
#' fiscal_year(dt)
#' fiscal_year(dt, fiscal_start = 7)
#' fiscal_year(dt, type = "date_first", fiscal_start = 7)
#' fiscal_year(dt, type = "date_last", fiscal_start = 7)
#' ffy(dt)
#' sfy_il(dt)
#'
#' @export
fiscal_year <- function(x,
                        type = c("year", "date_first", "date_last"),
                        fiscal_start = 1) {
  type <- match.arg(type)
  if (fiscal_start == 1 | lubridate::month(x) < fiscal_start) {
    fyear <- lubridate::year(x)
  } else {
    fyear <- lubridate::year(x) + 1
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


#' Format date-time to string without leading zeros
#'
#' Converts a date-time object to a character string without leading zeros in numeric components. Wraps `format.POSIXlt()`, removing leading zeros using regex substitution.
#'
#' @param x A date-time object.
#' @param format A character string giving a date-time format as used by `strftime()`.
#' @param tz A character string specifying the time zone to be used.
#' @param usetz A logical value indicating whether the time zone abbreviation should be appended to the output.
#' @param ... Further arguments to be passed to `format.POSIXlt()`.
#'
#' @return A character vector representing the date-time without leading zeros.
#'
#' @examples
#' dt <- as.POSIXct("2023-06-05 01:02:03")
#'
#' # With leading zeros
#' format(dt, "%m/%d/%Y %H:%M:%S")
#'
#' # Without leading zeros
#' strftime_no_lead(dt, "%m/%d/%Y %H:%M:%S")
#'
#' @export
strftime_no_lead <- function(x,
                             format = "%m/%d/%Y",
                             tz = "",
                             usetz = FALSE,
                             ...) {
  gsub(
    "(?<!\\d)0",
    "",
    format(as.POSIXlt(x, tz = tz), format = format, usetz = usetz, ...),
    perl = TRUE
  )
}

# bugfix -- changed `is.null()` to `missing()` in second `if` statement
t_tibble <- function(x, names_to = "Variable", names_from = NULL) {
  if (!missing(names_from)) {
    names.t <- dplyr::pull(x, {{names_from}})
    x <- dplyr::select(x, !{{names_from}})
  }
  x.t <- t(x)
  if (!missing(names_from))
    colnames(x.t) <- names.t
  tibble::as_tibble(x.t, rownames = names_to, .name_repair = "unique")
}


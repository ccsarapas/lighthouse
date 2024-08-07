% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date.R
\name{strftime_no_lead}
\alias{strftime_no_lead}
\title{Format date-time to string without leading zeros}
\usage{
strftime_no_lead(
  x,
  format = "",
  tz = "",
  usetz = FALSE,
  no_lead = c("\%d", "\%m", "\%H", "\%I", "\%F", "\%r", "\%R", "\%T"),
  ...
)
}
\arguments{
\item{x}{a Date, date-time, or other object coercible to \code{"POSIXlt"}.}

\item{format}{a character string. If \code{""} (the default),
\code{"\%Y-\%m-\%d \%H:\%M:\%S"} will be used if any element has a time component
which is not midnight, and \code{"\%Y-\%m-\%d"} otherwise. If
\code{options("digits.secs")} is set, up to the specified number of digits will
be printed for seconds.}

\item{tz}{a character string specifying the time zone to be used for
    the conversion.  System-specific (see \code{\link[base]{as.POSIXlt}}), but
    \code{""} is the current time zone, and \code{"GMT"} is UTC.
    Invalid values are most commonly treated as UTC, on some platforms with
    a warning.}

\item{usetz}{logical.  Should the time zone abbreviation be appended
    to the output?  This is used in printing times, and more reliable
    than using \code{"\%Z"}.}

\item{no_lead}{a character vector of POSIX conversion specifications from
which leading 0s should be removed.}

\item{...}{further arguments to be passed from or to other methods.}
}
\value{
A character vector representing the date or date-time.
}
\description{
This wrapper around \code{\link[base:strptime]{base::strftime()}} converts a date or date-time object
to character without leading zeros in specified components.
}
\examples{
dt <- as.POSIXct("2023-06-05 01:02:03")

# with leading zeros
strftime(dt, "\%m/\%d/\%Y \%H:\%M:\%S")

# without leading zeros
strftime_no_lead(dt, "\%m/\%d/\%Y \%H:\%M:\%S")

}
\seealso{
\code{\link[base:strptime]{base::strftime()}}
}

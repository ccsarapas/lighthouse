% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restructure.R
\name{add_empty_rows}
\alias{add_empty_rows}
\title{Add empty rows}
\usage{
add_empty_rows(.data, .before = NULL, .after = NULL, .nrows = 1)
}
\description{
Adds the number of empty rows passed to \code{.nrows} (default 1) at positions
passed to \code{.before} or \code{.after}. Vectorized over \code{.before}, \code{.after}, and
\code{.nrows}.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{try_numeric}
\alias{try_numeric}
\alias{try.numeric}
\title{Suppress NA warning when coercing to numeric}
\usage{
try_numeric(x)

try.numeric(x)
}
\description{
Coerces \code{x} to numeric. If \code{x} cannot be coerced, returns \code{NA} and suppresses
coercion warning.
}

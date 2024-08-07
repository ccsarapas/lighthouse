% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{se}
\alias{se}
\title{Compute the standard error - DEPRECATED}
\usage{
se(x, na.rm = FALSE)
}
\arguments{
\item{x}{a numeric vector or a non-factor object coercible to numeric by \code{as.double(x)}.}

\item{na.rm}{logical. Should missing values be removed?}
}
\description{
Deprecated in 0.7.0 favor of the more specific functions \code{\link[=se_mean]{se_mean()}} and \code{\link[=se_mean]{se_mean()}}. \code{se()} now calls \code{se_mean()} with a deprecation warning.

Computes the standard error of the values in x.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{row_sums_spss}
\alias{row_sums_spss}
\title{Replicate SPSS SUM() function - DEPRECATED}
\usage{
row_sums_spss(...)
}
\description{
Deprecated in lighthouse 0.7.2 in favor of \code{\link[=psum]{psum()}}.

Sums across columns a la SPSS: \code{NA}s are counted as 0s, but if ALL variables
are \code{NA}, then result is \code{NA}.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_summaries.R
\name{cols_info}
\alias{cols_info}
\title{Get information about data frame columns}
\usage{
cols_info(x, zap_spss = TRUE)
}
\arguments{
\item{x}{A data frame.}

\item{zap_spss}{If \code{TRUE} (default) and the \"labelled\" package is available,
convert SPSS-style labeled columns to standard R columns. Ignored if
\"labelled\" is not installed.}
}
\value{
A tibble with a row for each column in \code{x}, containing:
\itemize{
\item \code{column}: Column name
\item \code{class}: Column class
\item \code{type}: Column type
\item \code{valid_n}: Number of non-missing values
\item \code{valid_pct}: Percentage of non-missing values
\item \code{label}: SPSS variable label (if applicable)
}
}
\description{
Returns a summary of each column's class, type, and missing data. If the
data frame was imported from an SPSS .sav file and the \"labelled\" package is
installed, SPSS variable labels will also be included.
}
\examples{
cols_info(dplyr::starwars)

}

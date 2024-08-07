% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{untidyselect}
\alias{untidyselect}
\title{Convert a tidy selection to a vector of column names}
\usage{
untidyselect(data, selection, syms = FALSE)
}
\description{
Returns all column names selected by a <\code{\link[=dplyr_tidy_select]{tidy-select}}>
expression as a character vector (default) or a list of symbols (if \code{syms} =
\code{TRUE}).
}
\examples{
dplyr::storms \%>\%
  untidyselect(c(name:hour, category, tidyselect::ends_with("diameter")))

 mtcars \%>\%
   untidyselect(mpg:drat, syms = TRUE)

}

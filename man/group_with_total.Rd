% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_summaries.R
\name{group_with_total}
\alias{group_with_total}
\title{Add "Total" group to grouped dataframe}
\usage{
group_with_total(
  .data,
  ...,
  .totals_for = NULL,
  .label = "Total",
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data),
  .first_row = FALSE
)
}
\description{
Groups a dataframe by columns specified in \code{...} using \code{dplyr::group_by()},
and adds an additional group containing all observations. Useful for
including a "total" or "overall" row in summaries.

If more than one column is passed to \code{...}, the "total" group will combine
all groups in the first column passed, unless a different column is specified
in \code{.totals_for}.

Removing or changing the grouping structure after calling
\code{group_with_total()} but before aggregating may yield inaccurate results.
}
\examples{
ggplot2::mpg \%>\%
  group_with_total(class) \%>\%
  dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))

ggplot2::mpg \%>\%
  group_with_total(year, drv, .label = "all years") \%>\%
  dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))

ggplot2::mpg \%>\%
  group_with_total(year, drv, .totals_for = drv) \%>\%
  dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))

}

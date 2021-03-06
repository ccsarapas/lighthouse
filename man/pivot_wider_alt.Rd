% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{pivot_wider_alt}
\alias{pivot_wider_alt}
\title{Alternative column ordering and naming for pivot_wider()}
\usage{
pivot_wider_alt(
  data,
  id_cols = NULL,
  names_from = name,
  sort_by_col = TRUE,
  names_value_first = TRUE,
  names_value_sep = ".",
  names_sep = "_",
  names_prefix = "",
  names_glue = NULL,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL
)
}
\arguments{
\item{data}{A data frame to pivot.}

\item{id_cols}{<\code{\link[tidyr:tidyr_tidy_select]{tidy-select}}> A set of columns that
uniquely identifies each observation. Defaults to all columns in \code{data}
except for the columns specified in \code{names_from} and \code{values_from}.
Typically used when you have redundant variables, i.e. variables whose
values are perfectly correlated with existing variables.}

\item{names_from}{<\code{\link[tidyr:tidyr_tidy_select]{tidy-select}}> A pair of
arguments describing which column (or columns) to get the name of the
output column (\code{names_from}), and which column (or columns) to get the
cell values from (\code{values_from}).

If \code{values_from} contains multiple values, the value will be added to the
front of the output column.}

\item{sort_by_col}{If \code{TRUE} (default), output columns will be sorted by
\code{names_from}, then \code{values_from}. (Differs from \code{tidyr::pivot_wider()},
which sorts by \code{values_from} first, then \code{names_from}.)}

\item{names_value_first}{If \code{FALSE}, output columns will be named using a
\verb{\{column\}_\{.value\}} scheme. (Differs from \code{tidyr::pivot_wider()}, which uses
a \verb{\{.value\}_\{column\}} scheme.)}

\item{names_value_sep, names_sep}{If \code{names_from} or \code{values_from} contain
multiple variables, these will be used to join their values together into a
single string to use as a column name. \code{names_value_sep} will separate
\code{{.value}} from \code{{column}} components, while \code{names_sep} will separate
\code{{column}} components from one another if \code{names_from} contains multiple
variables. See Details and Examples.}

\item{names_prefix, names_glue, names_repair, values_from, values_fill, values_fn}{See documentation at \code{\link[=pivot_wider]{tidyr::pivot_wider()}}.}
}
\description{
A wrapper around \code{\link[=pivot_wider]{tidyr::pivot_wider()}} with additional
options for sorting and naming of output columns, with arguments
\code{sort_by_col}, \code{names_value_first}, and \code{names_value_sep} These options are
only relevant when more than one input column is passed to \code{values_from}.
}
\details{
The \code{names_value_sep} argument allows output column names that use a
different separator between \code{{.value}} and \code{{column}} than between multiple
\code{{columns}}s. Example:\preformatted{pivot_wider_alt(
  fakedata,
  names_from = c(size, color), # size = "sm", "med", "lg"; color = "red", "blue"
  values_from = c(n, weight),
  names_sep = "_",
  names_value_sep = ": "
)

# output column names:
# `n: sm_red`, `weight: sm_red`, `n: sm_blue`, `weight: sm_blue`, `n: med_red`...
}
}
\examples{
data_ex <- ggplot2::diamonds \%>\%
  dplyr::group_by(cut, color) \%>\%
  dplyr::summarize(Min = min(price), Median = median(price), Max = max(price))

# default pivot_wider() behavior
data_ex \%>\%
  tidyr::pivot_wider(
    id_cols = color,
    names_from = cut,
    values_from = Min:Max
  )

# pivot_wider_alt() behavior
data_ex \%>\%
  pivot_wider_alt(
    id_cols = color,
    names_from = cut,
    values_from = Min:Max
)

# with `names_value_first` = FALSE
data_ex \%>\%
  pivot_wider_alt(
    id_cols = color,
    names_from = cut,
    values_from = Min:Max,
    names_value_first = FALSE
  )

# multiple `names_from` vars, with different value vs. name separators
ggplot2::mpg \%>\%
  dplyr::filter(class \%in\% c("compact", "subcompact", "midsize")) \%>\%
  dplyr::group_by(
    manufacturer,
    trans = stringr::str_extract(trans, ".*(?=\\\\()"), year
  ) \%>\%
  dplyr::summarize(across(c(cty, hwy), mean)) \%>\%
  pivot_wider_alt(
    names_from = trans:year,
    values_from = cty:hwy,
    names_sep = "_",
    names_value_sep = ": "
  )

}

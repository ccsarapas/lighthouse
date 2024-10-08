% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_summaries.R
\name{summary_report}
\alias{summary_report}
\alias{nom}
\alias{bin}
\alias{cont}
\title{Summarize variables based on measurement level}
\usage{
summary_report(
  .data,
  ...,
  .default = c("auto", "nom", "bin", "cont"),
  .drop = TRUE,
  .cont_fx = list(mean, sd),
  .missing_label = NA,
  na.rm = FALSE,
  na.rm.nom = na.rm,
  na.rm.bin = na.rm,
  na.rm.cont = na.rm
)

nom(...)

bin(...)

cont(...)
}
\arguments{
\item{.data}{a data frame or data frame extension.}

\item{...}{<\code{\link[=dplyr_tidy_select]{tidy-select}}> one or more variable names.
and/or tidyselect expressions. Elements may be wrapped in \code{nom()}, \code{bin()},
or \code{cont()} to force summarizing as binary, nominal, or continuous,
respectively; see details.}

\item{.default}{how to determine measurement level for variables if not
specified by a measurement wrapper. \code{"auto"} will guess measurement level
for each variable, while \code{"nom"}, \code{"bin"}, and \code{"cont"} will treat all
unwrapped variables as nominal, binary, or continuous, respectively.}

\item{.drop}{if \code{FALSE}, frequencies for nominal variables will include
counts for empty groups (i.e. for levels of factors that don't exist in the
data).}

\item{.cont_fx}{a list containing the two functions with which continuous
variables will be summarized.}

\item{.missing_label}{label for missing values in nominal variables.}

\item{na.rm}{if \code{TRUE}, \code{NA} values in each variable will be dropped prior to
computation.}

\item{na.rm.nom, na.rm.bin, na.rm.cont}{control \code{NA} handling specifically for
nominal, binary, or continuous variables. Overrides \code{na.rm} for that
variable type.}
}
\value{
A tibble with four columns:
\itemize{
\item \code{Variable}: Variable name
\item \code{Value}:
\itemize{
\item For nominal variables, a row for each unique value (including unobserved factor levels if \code{.drop = FALSE}).
\item For binary variables, either \code{TRUE} or \code{1} (for logical or numeric variables, respectively).
\item For continuous variables, the names of the summary statistics specified in \code{.cont_fx}.
}
\item \code{V1}:
\itemize{
\item For nominal and binary variables, the number of observations with the value in \code{Value}.
\item For continuous variables, the value of the first summary statistic.
}
\item \code{V2}:
\itemize{
\item For nominal and binary variables, the proportion of observations with the value in \code{Value}.
\item For continuous variables, the value of the second summary statistic.
}
}
}
\description{
Summarizes each variable passed to \code{...}. This is handled differently based
on each variable's level of measurement:
\itemize{
\item For nominal variables, returns \emph{n} and proportion for each level
\item For binary variables, returns \emph{n} and proportion \code{TRUE}
\item For continuous variables, returns mean and standard deviation by
default. Specify alternative summary statistics using \code{.cont_fx}.
}

By default, \code{summary_report()} will guess the measurement level for each
variable. This can be overridden for all variables using the \code{.default}
argument, or for select variables using the \code{nom()}, \code{bin()}, or \code{cont()}
measurement wrappers. See details.
}
\section{Determining measurement level}{

The measurement level for each variable is determined as follows:
\enumerate{
\item Variables wrapped in \code{nom()}, \code{bin()}, or \code{cont()} will be treated
as nominal, binary, or continuous, respectively.
\item Variables without a measurement wrapper will be treated as the type
specified in \code{.default}.
\item If \code{.default} is \code{"auto"}, measurement level will be inferred:
\itemize{
\item Logical vectors will be treated as binary if there are no missing
values or if \code{na.rm.bin = TRUE}.
\item Character vectors, factors, dates and datetimes, and logical
vectors with missing values will be treated as nominal.
\item All other variables will be treated as continuous.
}
}
}

\section{Support for binary variables}{

To be treated as binary, both of these must be true:
\enumerate{
\item The variable must be either a logical vector, or a binary numeric
vector containing only 0s and 1s.
\item The variable must not include any missing values, or \code{na.rm.bin}
must be set to \code{TRUE}.
}
Future extensions may allow handling of other dichotomous variables (e.g.,
\code{"Pregnant"} vs. \code{"Not pregnant"}), but this is not currently supported.
Instead, consider converting these to a logical indicator, e.g., \code{Pregnant = PregnancyStatus == "Pregnant"}.
}

\examples{
mtcars \%>\% 
  transform(high_hp = hp > 200) \%>\%  # create logical indicator
  summary_report(
    nom(cyl),  # numeric - would autotype as continuous, but override with `nom()`
    bin(am),   # numeric - would autotype as continuous, but override with `bin()`
    high_hp,   # logical - will autotype as binary
    mpg,       # numeric - will autotype as continuous
    .cont_fx = list(median, IQR)
  )

ggplot2::msleep \%>\%
  transform(herbivore = vore == "herbi") \%>\%
  summary_report(
    conservation, # character - will be autotyped as nominal
    herbivore, # logical, but w/ NAs - so will autotype as nominal
    sleep_total, # numeric - will be autotyped as continuous
    sleep_rem, # ditto, but has NAs so results will be NA
    .missing_label = "(missing)"
  )

# repeat above, but tweak NA handling
ggplot2::msleep \%>\%
  transform(herbivore = vore == "herbi") \%>\%
    summary_report(
      conservation, # character - will be autotyped as nominal
      herbivore,    # logical, w/ NAs - but will autotype as binary thanks to `na.rm.bin = TRUE`
      sleep_total,  # numeric - will be autotyped as continuous
      sleep_rem,    # numeric w/ NAs - but will still give results thanks to `na.rm.cont = TRUE`,
      na.rm.bin = TRUE,
      na.rm.cont = TRUE,
      .missing_label = "(missing)"
    )

#' \dontrun{
# create a report using pre-processed SOR data
total_label <- "SOR-II Overall"
data_baseline \%>\%
  group_with_total(ServiceType, .label = total_label) \%>\%
  summary_report(
    Age, Gender, Race,
    bin(DAUseAlcohol, DAUseIllegDrugs, DAUseBoth),
    DAUseAlcoholDays, DAUseIllegDrugsDays, DAUseBothDays,
    DAUseAlcoholDaysOrdinal, DAUseIllegDrugsDaysOrdinal, DAUseBothDaysOrdinal,
    na.rm = TRUE,
    .drop = FALSE
  ) \%>\%
  pivot_wider(
    names_from = ServiceType,
    names_vary = "slowest",
    values_from = V1:V2
  ) \%>\%
  relocate(contains(total_label), .after = Value) \%>\%
  add_rows_at_value(Variable, Race, DAUseBoth, DAUseBothDays) \%>\%
  print_all()
}

}

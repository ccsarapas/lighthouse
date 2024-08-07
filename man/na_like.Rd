% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{na_like}
\alias{na_like}
\title{Generate NA values of appropriate type - DEPRECATED}
\usage{
na_like(x, factor_as_character = TRUE, match_length = FALSE)
}
\description{
This function was deprecated in lighthouse 0.7.0 because (1) it's always
been buggy and (2) its main purpose was to pass appropriate \code{NA}s to
\code{dplyr::if_else()} and \code{case_when()}, but this is no longer necessary with
\href{https://dplyr.tidyverse.org/news/index.html#vctrs-1-1-0}{changes introduced in dplyr v1.1.0}

Returns compatible \code{NA} based on \code{x}. This is usually of the same type as \code{x}
(e.g., \code{NA_real_} if \code{x} is a double vector). If \code{x} is a factor, will
return \code{NA_character_} if \code{factor_as_character = TRUE} (the default) and
\code{NA_integer_} otherwise.
}

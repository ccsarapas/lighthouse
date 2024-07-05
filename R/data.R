#' CHS holidays over a 20-year period
#'
#' A dataset containing dates of Chestnut Health System holidays from 2010-12-31
#' through 2030-12-31.
#'
#' @format A tibble with 140 rows and 2 variables:
#' \describe{
#'   \item{Date}{date holiday observed}
#'   \item{Holiday}{holiday name}
#' }
"holidays_chestnut"

#' Illinois state holidays over a 20-year period
#'
#' A dataset containing dates of State of Illinois holidays from 2010-12-31
#' through 2030-12-31.
#'
#' @format A tibble with 252 rows and 2 variables:
#' \describe{
#'   \item{Date}{date holiday observed}
#'   \item{Holiday}{holiday name}
#' }
"holidays_il"

#' US federal holidays over a 20-year period
#'
#' A dataset containing dates of United States federal holidays from 2010-12-31
#' through 2030-12-31.
#'
#' @format A tibble with 212 rows and 2 variables:
#' \describe{
#'   \item{Date}{date holiday observed}
#'   \item{Holiday}{holiday name}
#' }
#' @source \url{https://www.opm.gov/policy-data-oversight/pay-leave/federal-holidays/}
"holidays_us"

#' Missing codes for GAIN ABS
#'
#' Labelled missings used in GAIN datasets.
#'
#' @format A named numeric vector.
"gain_missing_codes"

#' US state and territory data
#'
#' @description
#' `state.terr.name` and `state.terr.abb` expand the built-in `state.name` and
#' `state.abb` vectors by adding US territories and the District of Columbia.
#' This includes:
#' \itemize{
#'   \item American Samoa
#'   \item District of Columbia
#'   \item Guam
#'   \item Northern Mariana Islands
#'   \item Puerto Rico
#'   \item Virgin Islands
#' }
#'
#' `state.terr.data` includes names, abbreviations, and FIPS codes for US states
#' and territories.
#'
#' @seealso
#' \code{\link[datasets]{state.name}}
#'
#' @name territory
"state.terr.name"
#'
#' @rdname territory
"state.terr.abb"
#'
#' @rdname territory
"state.terr.data"


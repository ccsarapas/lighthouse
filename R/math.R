#' Descending cumulative sum
#'
#' Returns a cumulative sum beginning with the last element of `x`.
#'
#' @examples
#' ggplot2::diamonds %>%
#'   dplyr::count(cut) %>%
#'   dplyr::mutate(
#'     or_worse = cumsum(n),
#'     or_better = cumsum_desc(n)
#'   )
#'
#' @export
cumsum_desc <- function(x) rev(cumsum(rev(x)))

#' Tidy row sums
#'
#' A variant of `base::rowSums()` that supports tidyselect expressions and
#' alternative default `NA` handling. By default, returns `NA` when `na.rm =
#' TRUE` and all selected columns are `NA`. This differs from `rowSums()`, which
#' returns `0` in this situation.
#'
#' @param ... <[`tidy-select`][dplyr_tidy_select]> columns to sum across.
#' @param na.rm Should missing values (including `NaN`) be removed?
#' @param if_all_na value to return when `na.rm = TRUE` and \emph{all} values in
#'   a row are `NA`. Setting to `0` will emulate behavior of `rowSums()`.
#'
#' @seealso
#' - [`row_sums_if_any()`] for another variant of `rowSums()`
#' - [`pmax_across()`] and [`str_c_tidy()`] for more tidyselect-friendly function variants
#'
#' @examples
#' df <- tibble::tibble(
#'   x = c(1, 2, NA, NA),
#'   y = c(5, NA, 7, NA),
#'   z = c(9, 10, 11, NA)
#' )
#'
#' # using tidyselect expressions - no `pick()` required
#' df %>%
#'   dplyr::mutate(
#'     rowSums = rowSums(pick(x:z)),
#'     row_sums_across = row_sums_across(x:z)
#'   )
#'
#' # alternative NA handling, and emulating `rowSums()` behavior
#' df %>%
#'   dplyr::mutate(
#'     rowSums = rowSums(pick(x:z), na.rm = TRUE),
#'     row_sums_across = row_sums_across(x:z, na.rm = TRUE),
#'     row_sums_across_0 = row_sums_across(x:z, na.rm = TRUE, if_all_na = 0)
#'   )
#'
#' @export
row_sums_across <- function (..., na.rm = FALSE, if_all_na = NA) {
  cols <- dplyr::pick(...)
  out <- rowSums(cols, na.rm = na.rm)
  if (na.rm) out <- dplyr::if_else(rowSums(is_valid(cols)) == 0, if_all_na, out)
  out
}

#' Sums, maxima and minima with alternative missing value handling
#'
#' Returns the sum, maximum, or minimum of input values, similar to
#' `base::sum()`, `min()`, and `max()`. Unlike these base functions, these
#' variants return `NA` when all values are `NA` and `na.rm = TRUE`.
#' (`base::sum()`, `min()`, and `max()` return `0`, `-Inf`, and `Inf`,
#' respectively, in this situation). Also unlike base functions, `na.rm` is
#' `TRUE` by default (since this is the typical use case).
#'
#' @param ... numeric, logical, or (for `max_if_any()` and `min_if_any()`) character vectors.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @examples
#' some_na <- c(1, 2, NA)
#' all_na <- c(NA, NA, NA)
#'
#' # unlike base functions, `na.rm = TRUE` by default
#' max(some_na)
#' max_if_any(some_na)
#'
#' # unlike base functions, returns 0 when `na.rm = TRUE` and all inputs are `NA`
#' sum(all_na, na.rm = TRUE)
#' sum_if_any(all_na)
#'
#' @name aggregate_if_any
#' @export
sum_if_any <- function(..., na.rm = TRUE) {
  aggregate_if_any(..., na.rm = na.rm, .fn = sum)
}
#' @rdname aggregate_if_any
#' @export
max_if_any <- function(..., na.rm = TRUE) {
  aggregate_if_any(..., na.rm = na.rm, .fn = max)
}
#' @rdname aggregate_if_any
#' @export
min_if_any <- function(..., na.rm = TRUE) {
  aggregate_if_any(..., na.rm = na.rm, .fn = min)
}

aggregate_if_any <- function(..., na.rm, .fn) {
  # 0.7.0 - changed to use .fn(..., na.rm = FALSE) to handle type coercion
  if (all(is.na(c(...)))) .fn(..., na.rm = FALSE) else .fn(..., na.rm = na.rm)
}


#' tidyselect-friendly parallel minima and maxima
#'
#' Wrappers around `base::pmin()` and `base::pmax()` that accept
#' <[`tidy-select`][dplyr_tidy_select]> expressions.
#'
#' @examples
#' # using `base::pmax()`
#' mtcars %>%
#'   dplyr::mutate(
#'     max_val = pmax(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
#'   )
#'
#' # using `pmax_across()`
#' mtcars %>%
#'   dplyr::mutate(max_val = pmax_across(mpg:carb))
#'
#' @export
pmax_across <- function(cols, na.rm = FALSE) {
  pminmax_across(rlang::enquo(cols), na.rm = na.rm, .fn = pmax)
}

#' @rdname pmax_across
#' @export
pmin_across <- function(cols, na.rm = FALSE) {
  pminmax_across(rlang::enquo(cols), na.rm = na.rm, .fn = pmin)
}

pminmax_across <- function(cols, na.rm, .fn) {
  col_names <- untidyselect(dplyr::cur_data(), !!cols, syms = TRUE)
  dplyr::cur_data() %>%
    dplyr::mutate(out = .fn(!!!col_names, na.rm = na.rm)) %>%
    dplyr::pull(out)
}

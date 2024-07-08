#' Suppress NA warning when coercing to numeric
#'
#' Coerces `x` to numeric. If `x` cannot be coerced, returns `NA` and suppresses
#' coercion warning.
#'
#' @export
try_numeric <- function(x) {
  if (is.factor(x)) {
    warning(
      "`x` is a factor and will be converted based on factor codes, not factor labels."
    )
  }
  suppress_warnings_if(as.numeric(x), "NAs introduced by coercion")
}

#' @rdname try_numeric
#' @export
try.numeric <- try_numeric

#' Reorder factor levels by sorting along multiple other variables.
#'
#' Reorders levels of `.f` based on variables passed to `...`, breaking ties
#' using each variable in the order passed.
#'
#' @export
fct_reorder_n <- function(.f, ..., .desc = FALSE) {
  # based on code from
  # https://github.com/billdenney/forcats/blob/reordern/R/reorder.R
  if (is.character(.f)) .f <- factor(.f)
  new_order <- order(..., decreasing = .desc)
  idx <- as.integer(.f)[new_order]
  forcats::lvls_reorder(.f, idx = idx[!duplicated(idx) & !is.na(idx)])
}

#' Results of case_when() as factor.
#'
#' Wrapper for [`dplyr::case_when()`][case_when], but result is a factor with
#' levels in same order passed to `...`. Returns an ordered factor if `.ordered`
#' is `TRUE`.
#'
#' @export
fct_case_when <- function(..., .ordered = FALSE) {
  levels <- purrr::map_chr(list(...), ~ rlang::f_rhs(.x))
  factor(dplyr::case_when(...), levels = levels, ordered = .ordered)
}

#' Swap column values, optionally based on condition
#'
#' `swap()` swaps values between two columns. `swap_if()` swaps values only in rows where a condition is met. `x` and `y` will be cast to a common type using `vctrs::vec_cast_common()`.
#'
#' @param x column to swap with `y`.
#' @param y column to swap with `x`.
#' @param cond a logical vector.
#' @param missing what to do if `cond` is `NA`.
#'   - `"NA"` replaces both `x` and `y` with `NA`.
#'   - `"keep"` keeps values in their original columns (as though `cond` were `FALSE`).
#'   - `"swap"` swaps values between `x` and `y` (as though `cond` were `TRUE`).
#'
#' @references
#' Based on Hadley's code in a \href{https://github.com/tidyverse/dplyr/issues/2149#issuecomment-258916706}{this dplyr GitHub issue}.
#'
#' @export
swap <- function(.data, x, y) {
  dplyr::mutate(
    .data,
    ..out_x.. = {{y}},
    {{y}} := {{x}},
    {{x}} := ..out_x..,
    ..out_x.. = NULL
  )
}
#' @rdname swap
#' @export
swap_if <- function(.data, cond, x, y, missing = c("NA", "keep", "swap")) {
  missing <- match.arg(missing)
  cond_fx <- switch(
    missing,
    `NA` = identity,
    keep = is_TRUE,
    swap = is_TRUE_or_NA
  )
  dplyr::mutate(
    .data,
    ..out_x.. = dplyr::if_else(cond_fx({{cond}}), {{y}}, {{x}}),
    ..out_y.. = dplyr::if_else(cond_fx({{cond}}), {{x}}, {{y}}),
    {{x}} := ..out_x..,
    {{y}} := ..out_y..,
    ..out_x.. = NULL,
    ..out_y.. = NULL
  )
}


#' Collapse factor levels with additional controls
#'
#' Collapses factor levels into manually defined groups like
#' [`forcats::fct_collapse()`][fct_collapse], with additional options to control behavior when
#' specified levels don't exist in the data and to order factor levels in the
#' order listed.
#'
#' @param .f A factor (or character vector).
#' @param ... A series of named character vectors. The levels in each vector
#'   will be replaced with the name.
#' @param other_level Value of level used for "other" values not named in `...`.
#'   If `NULL`, no extra level is created.
#' @param reorder If `TRUE`, the collapsed levels are ordered in the order they
#'   are listed in `...`, followed by `other_level` if specified, and then
#'   existing levels.
#' @param unknown_levels How to handle levels listed in `...` that aren't
#'   present in the input factor `.f`. Options are:
#'   - "ignore": ignore unknown levels without a warning (default),
#'   - "warn": issue a warning and then ignore unknown levels,
#'   - "error": raise an error.
#'
#' @return A factor with collapsed levels.
#'
#' @examples
#' f <- factor(c("a", "b", "c", "d", "e", "f"))
#'
#' fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"))
#' fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"), reorder = FALSE)
#' fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"), other_level = "other")
#' # `unknown_levels = "warn"` mirrors behavior of `forcats::fct_collapse()`
#' \donttest{
#' fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"), unknown_levels = "warn")
#' }
#'
#' @export
fct_collapse_alt <- function(.f,
                             ...,
                             other_level = NULL,
                             reorder = TRUE,
                             unknown_levels = c("ignore", "warn", "error")) {
  unknown_levels <- match.arg(unknown_levels)
  if (unknown_levels == "warn") {
    out <- forcats::fct_collapse(.f = .f, ..., other_level = other_level)
  } else {
    tmp <- purrr::quietly(forcats::fct_collapse)(.f = .f, ..., other_level = other_level)
    is_unknown_warning <- grepl("Unknown levels in `f`:", tmp$warnings)
    for (w in tmp$warnings[!is_unknown_warning]) warning(w)
    if (unknown_levels == "error" && any(is_unknown_warning)) {
      stop(tmp$warnings[is_unknown_warning][[1]])
    }
    out <- tmp$result
  }
  if (reorder) out <- factor(out, union(names(list(...)), levels(out)))
  out
}


#' Reverse key a numeric vector
#'
#' Reverses a numeric vector `x` by subtracting from `min` and adding `max`.
#' Observed minimum and maximum of `x` are used unless otherwise specified.
#'
#' @examples
#' reverse_key(1:5)
#' reverse_key(3:5)
#' reverse_key(3:5, min = 1, max = 5)
#'
#' @export
reverse_key <- function(x,
                        na.rm = FALSE,
                        max = NULL,
                        min = NULL) {
  if (is.null(max)) max <- max(x, na.rm = na.rm)
  if (is.null(min)) min <- min(x, na.rm = na.rm)
  max - x + min
}


#' Scaling and centering of vectors
#'
#' A wrapper around `base::scale()` that returns a vector instead of a matrix.
#'
#' @examples
#' # using base::scale()
#' scale(0:4)
#'
#' # using scale_vec()
#' scale_vec(0:4)
#'
#' @export
scale_vec <- function(x, center = TRUE, scale = TRUE) {
  scale(x, center = center, scale = scale)[, 1]
}

#' Scale based on median absolute deviation
#'
#' Scales a vector of values based on the median absolute deviation. Values may
#' be centered around the median (default), mean, or not centered. Compare to
#' `base::scale()`, which uses standard deviation and centers around the mean by
#' default.
#'
#' @export
scale_mad <- function(x,
                      center = c("median", "mean", "none"),
                      mad_constant = 1.4826) {
  center <- match.arg(center)
  ctr <- switch(
    center,
    median = stats::median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    none = 0
  )
  (x - ctr) / stats::mad(x, center = ctr, constant = mad_constant, na.rm = TRUE)
}

#' Winsorize extreme values
#'
#' Sets all values more than `max_dev` deviations from center to be `max_dev`
#' deviations from center. Deviations defined as standard deviation (the
#' default) or mean absolute deviation (if `method = "mad"`). Center defined as
#' mean for `method = "sd"` and median for `method = "mad"`, unless otherwise
#' specified in `center` argument.
#'
#' @export
winsorize <- function(x,
                      max_dev = 3,
                      method = c("sd", "mad"),
                      mad.center = c("median", "mean")) {
  method <- match.arg(method)
  if (method == "sd" && !missing(mad.center)) {
    warning('Argument `mad.center` ignored when `method` = "sd"')
  }
  if (method == "sd") {
    cent <- mean(x, na.rm = TRUE)
    dev <- stats::sd(x, na.rm = TRUE)
  } else {
    mad.center <- match.arg(mad.center)
    cent <- switch(
      mad.center,
      median = stats::median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE)
    )
    dev <- stats::mad(x, center = cent, na.rm = TRUE)
  }
  xmin <- cent - (max_dev * dev)
  xmax <- cent + (max_dev * dev)
  pmax(pmin(x, xmax), xmin)
}

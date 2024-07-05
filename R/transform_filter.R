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
#'
#' @export
try.numeric <- function(x) try_numeric(x)

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


#' Filter by and drop factor levels simultaneously
#'
#' Filters dataframe by specified levels of `.fct`, then drops filtered levels
#' from `.fct`.
#'
#' @export
fct_filter <- function(.data, .fct, .keep = NULL, .drop = NULL) {
  if (!xor(is.null(.keep), is.null(.drop))) {
    stop("Must specify either .keep or .drop (not both).")
  } else if (!is.null(.keep)) {
    .data %>%
      dplyr::filter({{.fct}} %in% .keep) %>%
      dplyr::mutate({{.fct}} := forcats::fct_drop({{.fct}}))
  } else {
    # should rewrite this to drop only levels in .drop
    .data %>%
      dplyr::filter(!({{.fct}} %in% .drop)) %>%
      dplyr::mutate({{.fct}} := forcats::fct_drop({{.fct}}))
  }
}

#' Filter by and drop a column simultaneously
#'
#' Filters dataframe by specified values of `.col`, then drops `.col`.
#'
#' @export
filter_drop <- function(.data, .col, .keep = NULL, .drop = NULL) {
  if (!xor(is.null(.keep), is.null(.drop))) {
    stop("Must specify either .keep or .drop (not both).")
  } else if (!is.null(.keep)) {
    .data %>%
      dplyr::filter({{.col}} %in% .keep) %>%
      dplyr::select(!{{.col}})
  } else {
    .data %>%
      dplyr::filter(!({{.col}} %in% .drop)) %>%
      dplyr::select(!{{.col}})
  }
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


#'  Swap column values based on condition
#'
#'  Swaps values between two columns if condition is met. Based on Hadley's code
#'  at: https://github.com/tidyverse/dplyr/issues/2149#issuecomment-258916706
#'
#' @export
swap_if <- function(.data, cond, x, y) {
  .data %>%
    dplyr::mutate(
      out_x = dplyr::if_else({{cond}}, {{y}}, {{x}}),
      out_y = dplyr::if_else({{cond}}, {{x}}, {{y}}),
      {{x}} := out_x,
      {{y}} := out_y
    ) %>%
    dplyr::select(!out_x:out_y)
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


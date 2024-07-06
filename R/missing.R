
#' Count non-missing cases
#'
#' @description
#' `n_valid()` returns the number of vector elements that are not `NA`. It
#' returns the percentage of non-`NA` values if `out` = `"pct"`, and a tibble
#' containing number and percentage if `out` = `"n_pct"`.
#'
#' `pct_valid()` is a wrapper around `n_valid(out = "pct")`.
#'
#' `n_pct_valid()` is a wrapper around `n_pct_valid(out = "n_pct")`.
#'
#' @export
n_valid <- function(x, out = c("n", "pct", "n_pct"), ...) {
  n <- sum(!is.na(x))
  switch(
    match.arg(out),
    n = n,
    pct = n / length(x),
    n_pct = tibble::tibble(valid_n = n, valid_pct = n / length(x))
  )
}

#' @rdname n_valid
#'
#' @export
pct_valid <- function(x, ...) n_valid(x, "pct")

#' @rdname n_valid
#'
#' @export
n_pct_valid <- function(x, ...) n_valid(x, "n_pct")


#' Remove missing values
#'
#' Returns a vector with `NA`s removed. Similar to `stats::na.omit.default()`, but does not add attributes to returned value.
#'
#' @export
discard_na <- function(x) x[!is.na(x)]


#' Identify or remove columns with no data
#'
#' `find_na_cols()` returns the names of all columns in `.data` where all values
#' are `NA`. `drop_na_cols()` returns the dataset with all-`NA` columns removed.
#'
#' @param .data A data frame or data frame extension (e.g. a tibble).
#'
#' @param cols <[`tidy-select`][dplyr_tidy_select]> Columns to check.
#'
#' @param quietly If `FALSE`, will print which columns were dropped.
#'
#' @export
find_na_cols <- function(.data, cols = tidyselect::everything()) {
  names(dplyr::select(
    .data,
    {{cols}} & where(~ all(is.na(.x)))
  ))
}

#' @rdname find_na_cols
#'
#' @export
drop_na_cols <- function(.data, cols = tidyselect::everything(), quietly = FALSE) {
  dropped <- find_na_cols(.data, cols = {{cols}})
  if (!quietly) {
    message(glue::glue(
      "Dropped {length(dropped)} columns with all NA values: ",
      "{str_collapse(dropped, sep = ', ')}"
    ))
  }
  dplyr::select(.data, !dplyr::any_of(dropped))
}

#' Drop rows where all columns are `NA`
#'
#' Drops rows where all specified columns are `NA`. If no columns are specified, all columns will be considered.
#'
#' @param data A data frame.
#' @param ... (Optional) Columns to test for `NA`s. If not specified, all columns will be considered.
#'
#' @return A data frame with rows removed that contain `NA` values across all (specified) columns.
#'
#' @examples
#' dat <- tibble::tibble(
#'   x = c(NA, NA, 3),
#'   y = c(NA, NA, 4),
#'   z = c(5, NA, NA)
#' )
#' drop_na_rows(dat)
#' drop_na_rows(dat, x, y)
#'
#' @export
drop_na_rows <- function(data, ...) {
  if (missing(...)) {
    na_mat <- is.na(data, ...)
  } else {
    na_mat <- is.na(dplyr::select(data, ...))
  }
  data[rowSums(na_mat) != ncol(na_mat), ]
}

#' Identify non-missing values
#'
#' A wrapper around `!is.na(x)`.
#'
#' @export
is_valid <- function(x) !is.na(x)

#' @rdname is_valid
#'
#' @export
is.valid <- is_valid


#' Replace NA with NULL and vice versa
#'
#' @description
#' `na_to_null()` Replaces all `NA`s in a vector or list with `NULL`. Can be
#' useful for lists of function arguments (e.g., when using `purrr::pmap()`).
#'
#' `null_to_na()` Replaces all `NULL`s in a list with `NA`s. Returns an atomic
#' vector if `unlist = TRUE` and a list otherwise.
#'
#' @export
na_to_null <- function(x) purrr::map(x, ~ {if (is.na(.x)) NULL else .x})

#' @rdname na_to_null
#'
#' @export
null_to_na <- function(x, unlist = FALSE) {
  out <- purrr::map(x, ~ {if (is.null(.x)) NA else .x})
  if (unlist) unlist(out) else out
}

#' Set NA values based on range of numbers.
#'
#' Changes values in range `range_min` to `range_max` to `NA`. Works on numeric
#' vectors as well as for numbers in character vectors, factor labels, or in
#' numeric or character vectors with classes `labelled` or `haven_labelled`.
#'
#' @param x A numeric vector, character vector, or factor.
#' @param range_min The minimum value to set to `NA`. Defaults to `-Inf`.
#' @param range_max The maximum value to set to `NA`. Defaults to `-1`.
#'
#' @note Previously known as `coerce_na_range`, which is retained as an alias for backward compatibility.
#'
#' @export
na_if_range <- function(x, range_min = -Inf, range_max = -1) UseMethod("na_if_range")
#' @rdname na_if_range
#' @export
na_if_range.numeric <- function(x, range_min = -Inf, range_max = -1) {
  dplyr::if_else(x < range_min | x > range_max, x, NA)
}
#' @rdname na_if_range
#' @export
na_if_range.character <- function(x, range_min = -Inf, range_max = -1) {
  coerced <- try_numeric(x)
  dplyr::if_else(
    is.na(coerced) | coerced < range_min | coerced > range_max,
    x,
    NA
  )
}
#' @rdname na_if_range
#' @export
na_if_range.factor <- function(x, range_min = -Inf, range_max = -1) {
  lvls <- levels(x)
  coerced <- try_numeric(as.character(lvls))
  lvls <- lvls[is.na(coerced) | coerced < range_min | coerced > range_max]
  factor(x, levels = lvls)
}
#' @rdname na_if_range
#' @export
na_if_range.labelled <- function(x, range_min = -Inf, range_max = -1) {
  rlang::check_installed("labelled")
  cls <- class(x)
  if (any(c("double", "integer", "numeric") %in% cls)) {
    x <- na_if_range.numeric(x, range_min, range_max)
  } else if ("character" %in% cls) {
    x <- na_if_range.character(x, range_min, range_max)
  } else {
    stop(
      "no applicable method for 'na_if_range' applied to an object of class \"c('",
      paste(cls, collapse = "', '"),
      "')\""
    )
  }
  labs <- labelled::val_labels(x)
  labelled::val_labels(x) <- labs[labs < range_min | labs > range_max]
  x
}
#' @rdname na_if_range
#' @export
na_if_range.haven_labelled <- na_if_range.labelled
#' @rdname na_if_range
#' @export
coerce_na_range <- na_if_range


#' Get the nth, first, or last non-`NA` value in a vector
#'
#' These functions retrieve the nth, first or last non-`NA` value in a vector. If there are fewer than `n` non-`NA` values, a default value can be returned.
#'
#' @param x A vector.
#' @param n integer. Position of non-`NA` value to return. Negative values start from the end of the vector.
#' @param default a default value to use if there are fewer than `n` non-`NA` values in `x`. Will be cast to the type of `x`.
#'
#' @return
#' - `nth_valid`: The nth non-`NA` value in `x`.
#' - `first_valid`: The first non-`NA` value in `x`.
#' - `last_valid`: The last non-`NA` value in `x`.
#'
#' @seealso{
#' \code{\link[dplyr]{nth()}}
#' }
#'
#' @examples
#' x <- c(NA, 7, NA, 5, 4, NA, 2, NA)
#'
#' first_valid(x)
#' last_valid(x)
#'
#' nth_valid(x, 2)
#' nth_valid(x, -2)
#'
#' nth_valid(x, 6)
#' nth_valid(x, 6, default = -Inf)
#'
#' @export
nth_valid <- function(x, n, default = NA) {
  dplyr::nth(discard_na(x), n = n, default = default)
}
#' @rdname nth_valid
#' @export
first_valid <- function(x, default = NA) nth_valid(x, n = 1L, default = default)
#' @rdname nth_valid
#' @export
last_valid <- function(x, default = NA) nth_valid(x, n = -1L, default = default)

#' Convert specified factor levels to `NA`
#'
#' Converts specified level(s) of a factor to `NA`, removing those levels from
#' the factor. This differs from the behavior of [`dplyr::na_if()`][na_if], which (1)
#' replaces values with `NA` but retains the associated factor level, and (2)
#' can replace only a single value.
#'
#' @param x A factor.
#' @param y A character vector of levels to convert to `NA`.
#'
#' @return The input factor with specified levels converted to `NA` and removed from the levels.
#'
#' @examples
#' f <- factor(c("a", "b", "c", "a"))
#' fct_na_if(f, "a")
#' fct_na_if(f, c("a", "c"))
#'
#' # compare `na_if()`
#' dplyr::na_if(f, "a")
#'
#' @seealso{
#' \code{\link[dplyr]{na_if}}
#' }
#'
#' @export
fct_na_if <- function(x, y) {
  x <- as.factor(x)
  factor(x, setdiff(levels(x), y))
}

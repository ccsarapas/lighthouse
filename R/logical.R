
#' Identify duplicates within a vector or vectors
#'
#' This function checks for duplicated values within a vector or a set of vectors.
#'
#' @param ... one or more vectors of equal length.
#' @param nmax maximum number of times a value can appear before being considered a duplicate.
#' @param incomparables should missing values (including `NaN`) be considered duplicates?
#'
#' @return a logical vector.
#'
#' @examples
#' x <- c(1, 2, 2, 3, 3, 3)
#' y <- c(1, 1, 2, 1, 2, 2)
#' is_duplicate(x)
#' is_duplicate(x, nmax = 2)
#' is_duplicate(x, y)
#'
#' z <- c(1, NA, NA)
#' is_duplicate(z)
#' is_duplicate(z, incomparables = TRUE)
#'
#' @export
is_duplicate <- function(..., nmax = 1, incomparables = FALSE) {
  if (length(unique(purrr::map_int(list(...), length))) != 1) {
    stop("All vectors passed to `...` must be of equal length.")
  }
  x <- suppress_messages_if(
    tibble::tibble(..., .name_repair = "unique"),
    "^New names:",
    fixed = FALSE
  )
  x <- dplyr::add_count(x, dplyr::across(tidyselect::everything()))
  if (!incomparables) {
    x <- dplyr::mutate(
      x,
      is_dup = !dplyr::if_any(!n, ~ is.na(.x) | is.nan(.x)) & n > nmax
    )
    x$is_dup
  } else {
    x$n > nmax
  }
}

#' Test whether a data frame contains SPSS variable or value labels
#'
#' Checks if a data frame contains any SPSS / haven variable labels, value labels, or format attributes.
#'
#' @param .data A data frame.
#'
#' @return `TRUE` if the data frame contains any SPSS labels, `FALSE` otherwise.
#'
#' @export
is_spss <- function(.data) {
  rlang::check_installed(c("haven", "labelled"))
  purrr::some(
    .data,
    \(x) !is.null(labelled::var_label(x)) | labelled::is.labelled(x) |
      "format.spss" %in% names(attributes(x))
  )
}

#' Test for data encoded as other formats
#'
#' Tests whether each element of a vector can be coerced to another type. See
#' examples.
#'
#' @param x Vector to be tested
#'
#' @param all If `TRUE`, returns a single logical indicating whether every
#' element of `x` is coercible. If `FALSE` (the default), returns a logical
#' vector the same length as `x` testing each element of `x`.
#'
#' @param na Should `NA` values test `NA` (the default) or `TRUE`?
#'
#' @param numeric For `is_coercible_logical`, should any numeric value test
#' `TRUE`, or only `0` and `1` (the default)?
#'
#' @examples
#' x <- c("1", "-1.23", "$1,234", NA)
#' is_coercible_numeric(x)
#'
#' is_coercible_numeric(x, na = "TRUE")
#'
#' is_coercible_numeric(x, all = TRUE)
#'
#' is_coercible_integer(x)
#'
#' y <- c("TRUE", "T", "F", "YES", "NA", NA)
#' is_coercible_logical(y)
#'
#' z <- c(0, 1, 2, .1, -1)
#' is_coercible_logical(z)
#'
#' is_coercible_logical(z, numeric = "any")
#'
#' @export
is_coercible_numeric <- function(x, all = FALSE, na = c("NA", "TRUE")) {
  out <- dplyr::if_else(
    is.na(x),
    as.logical(match.arg(na)),
    !is.na(try.numeric(x))
  )
  if (all) all(out) else out
}

#' @rdname is_coercible_numeric
#'
#' @export
is_coercible_integer <- function(x, all = FALSE, na = c("NA", "TRUE")) {
  na <- as.logical(match.arg(na))
  x_numeric <- try_numeric(x)
  out <- dplyr::case_when(
    is.na(x) ~ na,
    is.na(x_numeric) ~ FALSE,
    TRUE ~ dplyr::near(x_numeric, as.integer(x_numeric))
  )
  if (all) all(out) else out
}

#' @rdname is_coercible_numeric
#'
#' @export
is_coercible_logical <- function(x,
                                 all = FALSE,
                                 na = c("NA", "TRUE"),
                                 numeric = c("binary", "any")) {
  out <- dplyr::if_else(
    is.na(x),
    as.logical(match.arg(na)),
    !is.na(as.logical(x))
  )
  if (is.numeric(x) && match.arg(numeric) == "binary") {
    out <- dplyr::if_else(out & !is.na(x), x %in% c(0, 1), out)
  }
  if (all) all(out) else out
}


#' Vectorized logical tests
#'
#' `is_TRUE()` and `is_FALSE()` are vectorized versions of `base::isTRUE()` and
#' `base::isFALSE()`, respectively. `is_TRUE()` returns `TRUE` for each vector
#' element that evaluates to `TRUE`, and `FALSE` for all other elements
#' (including for `NA`s and non-logical values). Useful for handling `NA`s in
#' logical tests.
#'
#' @param x Vector to be tested
#'
#' @param strict If `TRUE` (the default), numeric and character types will
#' always return `FALSE`. If `FALSE`, numeric and character vectors that can be
#' coerced to logical (e.g., `1`, `"FALSE"`) will be coerced before testing.
#'
#' @export
is_TRUE <- function(x, strict = TRUE) {
  is.lgl <- if (strict) is_logical_vec else is_coercible_logical
  is.lgl(x) & !is.na(x) & x
}

#' @rdname is_TRUE
#'
#' @export
is_FALSE <- function(x, strict = TRUE) {
  is.lgl <- if (strict) is_logical_vec else is_coercible_logical
  is.lgl(x) & !is.na(x) & !x
}

#' @rdname is_TRUE
#'
#' @export
is_TRUE_or_NA <- function(x, strict = TRUE) {
  is.lgl <- if (strict) is_logical_vec else is_coercible_logical
  is.lgl(x) & (x | is.na(x))
}

#' @rdname is_TRUE
#'
#' @export
is_FALSE_or_NA <- function(x, strict = TRUE) {
  is.lgl <- if (strict) is_logical_vec else is_coercible_logical
  is.lgl(x) & (!x | is.na(x))
}

#' Test if two objects have the same shape
#'
#' Checks if two objects `x` and `y` have the same shape, i.e., the same dimensions or same length for vectors.
#'
#' @param x An object.
#' @param y An object.
#'
#' @return `TRUE` if `x` and `y` have the same shape, `FALSE` otherwise.
#'
#' @examples
#' eq_shape(1:5, 1:5)
#' eq_shape(1:5, 1:6)
#' eq_shape(matrix(1:6, nrow = 2), matrix(1:6, nrow = 3))
#' eq_shape(matrix(1:6, nrow = 2), matrix(1:6, nrow = 2))
#'
#' @export
eq_shape <- function(x, y) {
  dim_x <- dim(x)
  dim_y <- dim(y)
  if (is.null(dim_x) && is.null(dim_y)) {
    length(x) == length(y)
  } else {
    !is.null(dim_x) && !is.null(dim_y) && length(dim_x) == length(dim_y) && all(dim_x == dim_y)
  }
}


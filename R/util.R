

### consider deprecating? does it add anything over `str_c(x, collapse = "")`?
#' Collapse a character vector into a single string
#'
#' Concatenates all elements in a character vector. Essentially a column-wise
#' variant of dplyr::str_c(). Can accept multiple character vectors, which are
#' returned as separate character strings if `join` = NULL (the default), or
#' concatenated with a separator supplied to `join`.
#'
#' When only a single character vector is passed to `...`, behavior will be
#' similar to `stringr::str_c()` using the `collapse` argument. But behavior
#' differs when multiple vectors are passed to `...` -- see examples. The
#' difference arise because `str_collapse()` first collapses each vector, then
#' optionally joins the resulting vectors, whereas `stringr::str_c()` first
#' joins across vectors before collapsing the resulting vector.
#'
#' @param ... one or more character vectors
#' @param sep string to insert between elements of each input vector
#' @param join optional string to combine output into a single string. join multiple collapsed vectors. If NULL (default),
#'   returns a character vector with each input vector collapsed separately.
#'
#' @return
#' If `join` is NULL, returns a character vector with length equal to the number
#' of vectors passed to `...`. If `join` is provided, returns a character vector
#' of length 1.
#'
#' @examples
#' abc <- c("a", "b", "c")
#' def <- c("d", "e", "f")
#'
#' # with just a single character vector, behavior is similar to `stringr::str_c()`
#' # with the `collapse` argument
#' str_collapse(abc, sep = "-")
#' stringr::str_c(abc, collapse = "-")
#'
#' # but behavior differs when multiple vectors are passed
#' str_collapse(abc, def, sep = "-")
#' stringr::str_c(abc, def, collapse = "-")
#'
#' str_collapse(abc, def, sep = "-", join = " | ")
#' stringr::str_c(abc, def, collapse = "-", sep = " | ")
#' stringr::str_c(abc, def, sep = "-", collapse = " | ")
#'
#' @export
str_collapse <- function(..., sep = "", join = NULL) {
  out <- purrr::map_chr(list(...), \(x) stringr::str_c(x, collapse = sep))
  if (!is.null(join)) str_collapse(out, sep = join) else out
}








### consider deprecating? does it add much over `str_detect(string,
### paste(patterns, collapse = "|"))`, and does it introduce any problems?
#' Detect the presence of any pattern in a string
#'
#' Returns TRUE if any element of `patterns` is present in `string` (for
#' `str_detect_any()`) or at the start or end of `string` (for
#' `str_starts_any()` and `str_ends_any()`). Matches whole words if `whole_word`
#' = TRUE (FALSE by default). Case sensitive unless `ignore_case` = TRUE (FALSE
#' by default).
#'
#' @export
str_detect_any <- function(
    string,
    patterns,
    whole_word = FALSE,
    ignore_case = FALSE,
    negate = FALSE) {
  if (whole_word) {
    patterns <- stringr::str_c("\\b", patterns, "\\b")
  }
  if (ignore_case) {
    patterns <- stringr::str_to_lower(patterns)
    string <- stringr::str_to_lower(string)
  }
  pattern <- stringr::str_c(patterns, collapse = "|")
  stringr::str_detect(string = string, pattern = pattern, negate = negate)
}

### consider deprecating? as above. (if not, should have `str_ends_any()` too?)
#' Detect the presence of any pattern at the beginning of a string
#'
#' Returns TRUE if any element of `patterns` is present in `string`.
#'
#' @export
str_starts_any <- function(
    string,
    patterns,
    ignore_case = FALSE,
    negate = FALSE) {
  if (ignore_case) {
    string <- stringr::str_to_lower(string)
    patterns <- stringr::str_to_lower(patterns)
  }
  pattern <- str_collapse("(", patterns, ")", sep = "|", join = "")
  stringr::str_starts(string = string, pattern = pattern, negate = negate)
}

#### consider deprecating? I think the main impetus for this was
#### `dplyr::if_else()` being overly strict about type matching, which is
#### resolved now...
#' Median value as double
#'
#' Returns median of `x` as a double vector. Alternative to `stats::median()`
#' when a consistent return value is needed.
#'
#' @examples
#' \dontrun{
#' # stats::median raises error because of inconsistent return types
#' dplyr::if_else(c(TRUE, FALSE), median(1:4), median(1:5))
#' # Error in `dplyr::if_else()`:
#' # ! `false` must be a double vector, not an integer vector.
#'
#' # dplyr::if_else(c(TRUE, FALSE), median_dbl(1:4), median_dbl(1:5))
#' # 2.5 3.0
#' }
#'
#' @export
median_dbl <- function(x, na.rm = FALSE, ...) {
  stopifnot(typeof(x) %in% c("double", "integer", "logical"))
  as.double(stats::median(x, na.rm = na.rm, ...))
}

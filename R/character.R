
#' Find common prefixes or suffixes
#'
#' Returns substring beginnings or endings common to all elements in vector.
#'
#' @examples
#' test_words <- c("antidote", "antimony", "antimatter", "antisense")
#' str_prefix(test_words)
#'
#' wdays <- c(
#'   "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
#' )
#' str_suffix(wdays)
#'
#' @export
str_prefix <- function(string, na.rm = FALSE) {
  if (na.rm) string <- discard_na(string)
  lttrs <- stringr::str_split(string, "", simplify = TRUE)
  distinct <- apply(lttrs, MARGIN = 2, FUN = dplyr::n_distinct, simplify = TRUE)
  n_prefix <- min(which(distinct > 1)) - 1
  stringr::str_sub(string[[1]], end = n_prefix)
}

#' @rdname str_prefix
#'
#' @export
str_suffix <- function(string, na.rm = FALSE) {
  if (na.rm) string <- discard_na(string)
  string <- format(string, justify = "right")
  lttrs <- stringr::str_split(string, "", simplify = TRUE)
  distinct <- apply(lttrs, MARGIN = 2, FUN = dplyr::n_distinct, simplify = TRUE)
  n_suffix = length(distinct) - max(which(distinct > 1))
  stringr::str_sub(string[[1]], start = -n_suffix)
}


#' Format and interpolate a string as character vector
#'
#' A wrapper around `glue::glue()` which returns a character vector rather than
#' a `"glue"` object.
#'
#' @inheritParams glue::glue
#'
#' @export
glue_chr <- function(...) {
  as.character(glue::glue(..., .envir = rlang::caller_env()))
}


#' Concatenate strings with `NA` handling
#'
#' `str_c_narm` concatenates strings similar to `base::paste()` or `stringr::str_c()`, but with different `NA` handling. `NA`s are dropped row-wise prior to concatenation. See Details and Examples.
#'
#' @param ... character vectors or vectors coercible to character. May also be a single data frame (to accommodate `dplyr::across()` and `pick()`).
#' @param sep separator to insert between input vectors.
#' @param collapse optional character string to combine results into a single string.
#' @param if_all_na what to do if \emph{all} values in a row are `NA`:
#'   - `"empty"`: (default) returns an empty string.
#'   - `"NA"`: returns `NA`.
#'   - `"remove"`: removes the row from the output.
#'
#' @return A character vector.
#'
#' @details
#' `str_c_narm()` provides alternative missing value handling compared to
#' `base::paste()` and `stringr::str_c()`. Given some vectors with missing
#' values:
#' \preformatted{
#' #> (dat <- tibble::tibble(v1 = c("a1", "b1", "c1", NA),
#'                       v2 = c("a2", NA, "c2", NA),
#'                       v3 = c("a3", "b3", NA, NA)))
#' # A tibble: 4 × 3
#'   v1    v2    v3
#'   <chr> <chr> <chr>
#' 1 a1    a2    a3
#' 2 b1    NA    b3
#' 3 c1    c2    NA
#' 4 NA    NA    NA
#' }
#'
#' `paste()` and `paste0()` convert `NA`s to the string `"NA"` before concatenating:
#' \preformatted{
#' #> dplyr::mutate(dat, paste = paste(v1, v2, v3, sep = " | "))
#' # A tibble: 4 × 4
#'   v1    v2    v3    paste
#'   <chr> <chr> <chr> <chr>
#' 1 a1    a2    a3    a1 | a2 | a3
#' 2 b1    NA    b3    b1 | NA | b3
#' 3 c1    c2    NA    c1 | c2 | NA
#' 4 NA    NA    NA    NA | NA | NA
#' }
#'
#' ...while `stringr::str_c` returns `NA` if any value in a row is `NA`:
#' \preformatted{
#' #> dplyr::mutate(dat, str_c = stringr::str_c(v1, v2, v3, sep = " | "))
#' # A tibble: 4 × 4
#'   v1    v2    v3    str_c
#'   <chr> <chr> <chr> <chr>
#' 1 a1    a2    a3    a1 | a2 | a3
#' 2 b1    NA    b3    NA
#' 3 c1    c2    NA    NA
#' 4 NA    NA    NA    NA
#' }
#'
#' In contrast, `str_c_narm()` removes `NAs` from each row before concatenating:
#' \preformatted{
#' #> dplyr::mutate(dat, str_c_narm = str_c_narm(v1, v2, v3, sep = " | "))
#' # A tibble: 4 × 4
#'   v1    v2    v3    str_c_narm
#'   <chr> <chr> <chr> <chr>
#' 1 a1    a2    a3    "a1 | a2 | a3"
#' 2 b1    NA    b3    "b1 | b3"
#' 3 c1    c2    NA    "c1 | c2"
#' 4 NA    NA    NA    ""
#' }
#'
#' ...with options for the case where all values in a row are `NA`:
#' \preformatted{
#' #> dplyr::mutate(dat, str_c_narm = str_c_narm(v1, v2, v3, sep = " | ", if_all_na = "NA"))
#' # A tibble: 4 × 4
#'   v1    v2    v3    str_c_narm
#'   <chr> <chr> <chr> <chr>
#' 1 a1    a2    a3    a1 | a2 | a3
#' 2 b1    NA    b3    b1 | b3
#' 3 c1    c2    NA    c1 | c2
#' 4 NA    NA    NA    NA
#' }
#'
#' @examples
#' str_c_narm(c("a", NA), c("b", "c"), sep = "_")
#' str_c_narm(c("a", NA), c("b", NA), if_all_na = "NA")
#'
#' # compare behavior to `paste()` and `str_c()`
#' dat <- tibble::tibble(v1 = c("a1", "b1", "c1", NA),
#'                       v2 = c("a2", NA, "c2", NA),
#'                       v3 = c("a3", "b3", NA, NA))
#'
#' dplyr::mutate(
#'   dat,
#'   paste = paste(v1, v2, v3, sep = " | "),
#'   str_c = stringr::str_c(v1, v2, v3, sep = " | "),
#'   str_c_narm = str_c_narm(v1, v2, v3, sep = " | ")
#' )
#'
#' @export
str_c_narm <- function(...,
                       sep = "",
                       collapse = NULL,
                       if_all_na = c("empty", "NA", "remove")) {
  stop_dots <- function(...) {
    stop("`...` must contain atomic vectors of equal length or a single data.frame.")
  }
  dots <- list(...)
  if (any(sapply(dots, is.data.frame))) {
    if (length(dots) > 1L) stop_dots()
    strings <- dots[[1]]
  } else {
    if (!all(sapply(dots, is.atomic))) stop_dots()
    strings <- tryCatch(vctrs::vec_recycle_common(...), error = stop_dots)
  }
  strings <- sapply(strings, as.character)
  if (is.vector(strings)) strings <- matrix(strings, nrow = 1)
  out <- apply(strings, MARGIN = 1, \(x) stringr::str_c(x[!is.na(x)], collapse = sep))
  all_na_rows <- apply(strings, MARGIN = 1, \(x) all(is.na(x)))
  out <- switch(
    match.arg(if_all_na),
    empty = out,
    `NA` = ifelse(all_na_rows, NA, out),
    remove = out[!all_na_rows]
  )
  stringr::str_c(out, collapse = collapse)
}

#' Tidy string concatenation
#'
#' This function performs a tidyverse-friendly string concatenation. It takes a data frame or tibble and a selection of columns, concatenates the string values in each row, and returns the concatenated strings as a vector.
#'
#' @param .cols Columns to concatenate. Can be specified using tidyselect semantics.
#' @param sep Separator to use between concatenated values. Defaults to "".
#' @param collapse Untested, may give unpredictable results.
#' @param na.rm If `TRUE`, missing values will be removed prior to concatenation. Defaults to `FALSE`.
#'
#' @return A character vector of concatenated strings, one per row of the input data frame.
#'
#' @details
#' This function is designed to work seamlessly with tidyverse pipelines. It takes a data frame or tibble as input, and uses tidyselect semantics to choose the columns to concatenate. The resulting concatenated strings are returned as a vector, which can be easily added to the data frame using `dplyr::mutate()`.
#'
#' If `na.rm = TRUE`, the function will remove missing values before concatenation. It does this by temporarily replacing `NA` with a placeholder string, concatenating, and then removing the placeholder and any trailing separators.
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x, ~y, ~z,
#'   "a", "b", "c",
#'   "d", NA, "f",
#'   "g", "h", NA
#' )
#'
#' df %>% mutate(combined = str_c_tidy(x, y, z))
#' df %>% mutate(combined = str_c_tidy(starts_with("y"), z, na.rm = TRUE))
#'
#' @export
str_c_tidy <- function(.cols, sep = "", collapse = NULL, na.rm = FALSE) {
  # haven't tested with `collapse` arg, no idea how it would behave
  if (!is.null(collapse)) {
    warning(
      "The `collapse` argument is untested and may give unpredictable results."
    )
  }
  out <- dplyr::pick(everything())
  if (na.rm) {
    sep_out <- sep
    sep <- "__SEP__"
    out <- dplyr::mutate(out, dplyr::across(
      .cols,
      ~ if_else(
        if_all(.cols, is.na),
        NA_character_,
        str_replace_na(.x, "__NA__")
      )
    ))
  }
  out <- out %>%
    dplyr::mutate(joined = stringr::str_c(
      !!!untidyselect(., .cols, syms = TRUE),
      sep = sep,
      collapse = collapse
    )) %>%
    dplyr::pull(joined)
  if (na.rm) {
    out <- stringr::str_replace_all(out, c(
      "__SEP____NA__" = "",
      "__NA__(__SEP__)?" = "",
      "__SEP__" = sep_out
    ))
  }
  out
}

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
#' # with just a single character vector, behavior is similar to `stringr::str_c()`
#' # with the `collapse` argument
#' abc <- c("a", "b", "c")
#' str_collapse(abc, sep = "-")
#' stringr::str_c(abc, collapse = "-")
#'
#' # but behavior differs when multiple vectors are passed
#' def <- c("d", "e", "f")
#' str_collapse(abc, def, sep = "-")
#' stringr::str_c(abc, def, collapse = "-")
#'
#' str_collapse(abc, def, sep = "-", join = " | ")
#' stringr::str_c(abc, def, collapse = "-", sep = " | ")
#' stringr::str_c(abc, def, sep = "-", collapse = " | ")
#'
#' # can accept vectors of different lengths
#' lmnop <- c("l", "m", "n", "o", "p")
#' str_collapse(abc, def, lmnop, join = ", ")
#'
#' @export
str_collapse <- function(..., sep = "", join = NULL) {
  out <- purrr::map_chr(list(...), \(x) stringr::str_c(x, collapse = sep))
  if (!is.null(join)) str_collapse(out, sep = join) else out
}

#' Detect the presence of any pattern in a string
#'
#' `str_detect_any()` returns TRUE if any element of `patterns` is present in
#' `string`. `str_starts_any()` and `str_ends_any()` match only at the beginning
#' or end of strings.
#'
#' @param string Either a character vector, or something coercible to one.
#' @param patterns Character vector containing regular expressions to look for.
#' @param whole_word Logical. Match only whole words in `string` (as defined by `"\\b"` word boundary).
#' @param ignore_case Logical. Ignore case when matching `patterns` to `string`.
#' @param negate Logical. If `TRUE`, inverts the resulting boolean vector.
#'
#' @seealso{
#' \code{\link[stringr]{str_detect}}
#' \code{\link[stringr]{str_starts}}
#' }
#'
#' @name str_any
#' @export
str_detect_any <- function(string,
                           patterns,
                           whole_word = FALSE,
                           ignore_case = FALSE,
                           negate = FALSE) {
  str_any(string, patterns, whole_word, negate, fn = stringr::str_detect)
}
#'
#' @rdname str_any
#' @export
str_starts_any <- function(string,
                           patterns,
                           whole_word = FALSE,
                           ignore_case = FALSE,
                           negate = FALSE) {
  str_any(string, patterns, whole_word, negate, fn = stringr::str_starts)
}
#'
#' @rdname str_any
#' @export
str_ends_any <- function(string,
                         patterns,
                         whole_word = FALSE,
                         ignore_case = FALSE,
                         negate = FALSE) {
  str_any(string, patterns, whole_word, negate, fn = stringr::str_ends)
}

str_any <- function(string, patterns, whole_word, negate, fn) {
  if (ignore_case) {
    patterns <- stringr::str_to_lower(patterns)
    string <- stringr::str_to_lower(string)
  }
  if (whole_word) patterns <- stringr::str_c("\\b", patterns, "\\b")
  pattern <- stringr::str_c(patterns, collapse = "|")
  fn(string = string, pattern = pattern, negate = negate)
}

#' Print symbols as a character vector
#'
#' A helper for converting a list of symbols to a character vector. Primarily
#' intended to help convert old code given updates to `summary_report()`.
#'
#' @param ... symbols.
#' @param width maximum width of each line of output.
#' @param indent number of spaces to indent.
#' @param indent_first number of spaces to indent the first line.
#'
#' @examples
#' syms_to_chr(
#'   various, very, video, view, village, visit, vote, wage, wait, walk, wall,
#'   want, war, warm, wash, waste, watch, water, way, we, wear, wednesday, wee,
#'   week, weigh, welcome, well, west, what, when, where, whether, which, white,
#'   who, whole, why, wide, wife, will, win, wind, window, wish, with, within,
#'   without, woman, wonder, wood, word, work, world, worry, worse, worth, would,
#'   write, wrong, year, yes, yesterday, yet, you, young
#' )
#'
#' @export
syms_to_chr <- function(..., width = 80, indent = 0, indent_first = indent) {
  dots <- rlang::ensyms(...)
  out <- as.character(dots)
  out <- strwrap(
    paste0("c(", paste0('"', out, '"', collapse = ', '), ")"),
    width = width, indent = indent_first, exdent = indent + 2
  )
  cat(out, sep = "\n")
}

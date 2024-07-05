
#' Strip special attributes from SPSS dataset
#'
#' Removes all special attributes from data read from SPSS, optionally
#' converting labelled vectors to factors if `.as_factor` = TRUE (the default).
#'
#' @export
zap_everything <- function(.data, ..., .as_factor = TRUE) {
  rlang::check_installed("haven")
  .data <- .data %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      haven::zap_empty
    )) %>%
    haven::zap_missing() %>%
    haven::zap_label() %>%
    haven::zap_formats() %>%
    haven::zap_widths()
  if (.as_factor) {
    haven::as_factor(.data, ...)
  } else {
    haven::zap_labels(.data)
  }
}


#' Convert a tidy selection to a vector of column names
#'
#' Returns all column names selected by a <[`tidy-select`][dplyr_tidy_select]>
#' expression as a character vector (default) or a list of symbols (if `syms` =
#' `TRUE`).
#'
#' @examples
#' dplyr::storms %>%
#'   untidyselect(c(name:hour, category, tidyselect::ends_with("diameter")))
#'
#'  mtcars %>%
#'    untidyselect(mpg:drat, syms = TRUE)
#'
#' @export
untidyselect <- function(data, selection, syms = FALSE) {
  untidy <- names(
    tidyselect::eval_select(
      rlang::enquo(selection),
      data
    )
  )
  if (syms) rlang::syms(untidy) else untidy
}


#' Compute time period scores for GAIN-SS scales
#'
#' Pass all scale items to `...`. Will return columns with the score for
#' lifetime, past year, past 90 days, and past month positive items
#'
#' @export
gain_ss_score <- function(..., .prefix = NULL) {
  items <- dplyr::select(dplyr::cur_data(), ...)
  scores <- tibble::tibble(
    lifetime = rowSums(items > 0),
    pyear = rowSums(items > 1),
    p90d = rowSums(items > 2),
    pmonth = rowSums(items > 3)
  )
  if (is.null(.prefix)) {
    .prefix <- paste0("_", str_prefix(names(items)))
  }
  dplyr::rename_with(scores, ~ stringr::str_c(.prefix, .x))
}

#' Set comparison with automatic naming
#'
#' Compares two sets (vectors), returning the elements unique to each and the elements they have in common. Optionally names the returned sets based on the supplied object names.
#'
#' @param x A vector representing the first set.
#' @param y A vector representing the second set.
#' @param autoname Logical, whether to automatically name the returned sets based on the supplied object names. Defaults to `TRUE`.
#'
#' @return A named list with three elements:
#' - Elements unique to `x`
#' - Elements unique to `y`
#' - Elements common to both `x` and `y`
#'
#' @examples
#' v1 <- c(1, 2, 3, 4)
#' v2 <- c(3, 4, 5, 6)
#'
#' set_compare(v1, v2)
#' set_compare(v1, v2, autoname = FALSE)
#'
#' @export
set_compare <- function(x, y, autoname = TRUE) {
  if (autoname) {
    nm_x <- as.character(substitute(x))
    if (length(nm_x) != 1) nm_x <- "x"
    nm_y <- as.character(substitute(y))
    if (length(nm_y) != 1) nm_y <- "y"
    nms <- c(nm_x, nm_y, "intersect")
  } else {
    nms <- c("x", "y", "intersect")
  }
  out <- list(setdiff(x, y), setdiff(y, x), intersect(x, y))
  names(out) <- nms
  out
}


#' Conditionally suppress warnings or messages
#'
#' Evaluates an expression while selectively suppressing warnings or messages based on their content.
#'
#' @param expr The expression to evaluate.
#' @param msg_contains A string to match against the message. Defaults to an empty string (matches all messages).
#' @param fixed A logical indicating whether `msg_contains` should be matched as a fixed string..
#' @param perl A logical indicating whether Perl-compatible regular expressions should be used for `msg_contains`.
#' @param ignore.case A logical indicating whether the case of `msg_contains` should be ignored.
#' @param negate A logical indicating whether the message matching should be negated (e.g., suppress messages that do _not_ match `msg_contains`).
#' @param classes A character vector of warning or message classes to suppress.
#'
#' @return The result of evaluating `expr`, with specified warnings or messages suppressed.
#'
#' @examples
#' # Suppress warnings containing specific text
#' suppress_warnings_if(warning("This is a warning"), "This")
#'
#' # Suppress messages unless they contain specific text
#' suppress_messages_if(
#'   message("13 files processed"),
#'   "\\d{2, }",
#'   fixed = FALSE,
#'   negate = TRUE
#' )

#'
#' @name suppress_if
#' @export
suppress_warnings_if <- function(expr,
                                 msg_contains = "",
                                 fixed = TRUE,
                                 perl = !fixed,
                                 ignore.case = FALSE,
                                 negate = FALSE,
                                 classes = "warning") {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg_test <- grepl(
        msg_contains,
        w$message,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed
      )
      if (negate) msg_test <- !msg_test
      if (msg_test && inherits(w, classes)) rlang::cnd_muffle(w)
    }
  )
}
#'
#' @rdname suppress_if
#' @export
suppress_messages_if <- function(expr,
                                 msg_contains = "",
                                 fixed = TRUE,
                                 perl = !fixed,
                                 ignore.case = FALSE,
                                 negate = FALSE,
                                 classes = "message") {
  withCallingHandlers(
    expr,
    message = function(m) {
      msg_test <- grepl(
        msg_contains,
        m$message,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed
      )
      if (negate) msg_test <- !msg_test
      if (msg_test && inherits(m, classes)) rlang::cnd_muffle(m)
    }
  )
}

#' Open a file or directory
#'
#' Functions to open a file with its default program or open a file's location in the file explorer.
#'
#' `file.open()` and `dir.open()` are aliases, in line with `base::file.create`, `file.exists`, `dir.create`, `dir.exists`, etc.
#'
#' @param path The path to the file or directory.
#'
#' @section Functions:
#' - `open_file()`: Opens the file at the specified path using the default program for that file type. Automatically handles paths with special characters.
#' - `open_location()`: Opens the file explorer at the location of the specified file, with the file selected if possible. Automatically handles paths with special characters.
#'
#' @examples
#' \dontrun{
#' # Open a file
#' open_file("path/to/file.txt")
#'
#' # Open a file's location
#' open_location("path/to/file.txt")
#' }
#'
#' @export
open_file <- function(path) {
  tryCatch(shell.exec(path), error = \(e) shell.exec(normalizePath(path)))
}
#' @rdname open_file
#' @export
open_location <- function(path) {
  open_loc <- function(path) {
    suppress_warnings_if(
      shell(glue_chr('explorer /select,"{path}"')),
      "execution failed with error code 1"
    )
  }
  tryCatch(open_loc(normalizePath(path)), error = \(e) open_loc(path))
}
#' @rdname open_file
#' @export
file.open <- open_file
#' @rdname open_file
#' @export
dir.open <- open_location



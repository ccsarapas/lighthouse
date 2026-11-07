pairlist_auto_name <- function(...) {
  .fn_default_names <- purrr::map_chr(rlang::quos(...), rlang::as_label)
  .fns <- list(...)
  .fn_passed_names <- names(.fns)
  if (is.null(.fn_passed_names)) {
    names(.fns) <- .fn_default_names
  } else {
    names(.fns) <- ifelse(.fn_passed_names == "", .fn_default_names, .fn_passed_names)
  }
  .fns
}

is_logical_vec <- function(x) {
  stopifnot(rlang::is_vector(x))
  if (rlang::is_list(x)) {
    rep(is.logical(x), length(x))
  } else {
    purrr::map_lgl(x, is.logical)
  }
}

handle_sf <- function(x, quiet = FALSE) {
  if (inherits(x, "sf")) {
    x <- tibble::as_tibble(x)
    if (!quiet) {
      cli::cli_inform(
        "{.cls sf} object converted to {.cls tibble}.",
        .frequency = "once",
        .frequency_id = "lighthouse_handle_sf"
      )
    }
  }
  x
}

handle_list_cols <- function(x, caller, quiet = FALSE) {
  stopifnot(inherits(x, "data.frame"))
  idx_list_cols <- vapply(x, is.list, logical(1))
  if (any(list_cols)) {
    list_cols <- names(x)[idx_list_cols]
    x <- x[, !idx_list_cols, drop = FALSE]
    if (!quiet) {
      cli::cli_warn(c(
        "*" = "`{caller}()` does not support list columns.",
        "i" = "List column{?s} removed: {list_cols}."
      ))
    }
  }
  x
}

try.na.rm <- function(fn, x, na.rm = FALSE, ...) {
  if (any(c("na.rm", "...") %in% formalArgs(args(fn)))) {
    fn(x, na.rm = na.rm, ...)
  } else {
    fn(x, ...)
  }
}
## consider fx to support `.by` in functions (see similar code in `count_pct`, `count_multiple`, etc)

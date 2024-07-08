named_fn_list <- function(...) {
  .fn_default_names <- purrr::map_chr(rlang::quos(...), rlang::as_label)
  .fns <- list(...)
  .fn_passed_names <- names(.fns)
  if (is.null(.fn_passed_names)) {
    names(.fns) <- .fn_default_names
  } else {
    names(.fns) <- dplyr::coalesce(dplyr::na_if(.fn_passed_names, ""), .fn_default_names)
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

## consider fx to support `.by` in functions (see similar code in `count_pct`, `count_multiple`, etc)

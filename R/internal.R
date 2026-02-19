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

try.na.rm <- function(fn, x, na.rm = FALSE, ...) {
  if (any(c("na.rm", "...") %in% formalArgs(args(fn)))) {
    fn(x, na.rm = na.rm, ...)
  } else {
    fn(x, ...)
  }
}

do_by <- function(by, fn, .data, ...) {
  if (dplyr::is_grouped_df(.data) || inherits(.data, "rowwise_df")) {
      cli::cli_abort(
        "Can't supply `.by` when `.data` is a grouped or rowwise data frame."
      )
    }
  args <- rlang::enexprs(...)
  .drop <- args$.drop
  .data |>
    dplyr::group_by(
      dplyr::pick({{by}}), 
      .drop = .drop
    ) |>
    fn(...) |>
    dplyr::ungroup()
}


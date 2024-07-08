#' Nicer ggplot rendering - DEPRECATED
#'
#' @description
#' This function has been deprecated and is no longer maintained. Improvements
#' to RStudio graphics make it no longer needed.
#'
#' Saves ggplot object as .svg in R temp directory, then displays in RStudio
#' Viewer pane. Results in better image quality on Windows machines.
#'
#' @export
ggview <- function(plot = ggplot2::last_plot(),
                   width = NULL,
                   height = NULL,
                   type = c("svg", "png")) {
  .Deprecated(
    msg = "`ggview()` is deprecated. Improvements to R graphics on Windows have rendered it unnecessary."
  )
  device <- match.arg(type)
  img_dir <- file.path(tempdir(), "img")
  if (!dir.exists(img_dir)) {
    dir.create(img_dir)
  }
  img_path <- tempfile(tmpdir = img_dir, fileext = paste0(".", device))
  dev_dims <- grDevices::dev.size(units = "px")
  save_args <- list(
    filename = img_path,
    plot = plot,
    width = width %||% dev_dims[[1]] / 96,
    height = height %||% dev_dims[[2]] / 96,
    dpi = 96
  )
  if (device == "png") save_args[["type"]] <- "cairo"
  rlang::exec(ggplot2::ggsave, !!!save_args)
  viewer <- getOption("viewer")
  viewer(img_path)
}

#' Summarize a dataframe's column types - DEPRECATED
#'
#' @description
#' Deprecated in favor of [cols_info()], which provides more information and
#' features and is more stable.
#'
#' Returns class and type for each column in `.data`.
#'
#' @export
get_col_types <- function(.data) {
  .Deprecated("cols_info", old = "get_col_types")
  .data %>%
    dplyr::summarize(dplyr::across(
      .fns = list(class = class, type = typeof),
      .names = "{.col}__SEP__{.fn}"
    )) %>%
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("column", ".value"),
      names_sep = "__SEP__"
    )
}



#' Replicate SPSS SUM() function - DEPRECATED
#' @description
#' Deprecated in lighthouse 0.7.0 in favor of [row_sums_across()], which provides more
#' information and features and is more stable.
#'
#' Sums across columns a la SPSS: `NA`s are counted as 0s, but if ALL variables
#' are `NA`, then result is `NA`.
#'
#' @export
row_sums_spss <- function(...) {
  .Deprecated("row_sums_across", old = "row_sums_spss")
  sums <- tibble::tibble(...) %>%
    dplyr::mutate(
      out = dplyr::if_else(
        dplyr::if_all(tidyselect::everything(), is.na),
        NA_real_,
        rowSums(dplyr::across(), na.rm = TRUE)
      )
    )
  sums$out
}

#' Maxima and minima with alternative missing value handling - DEPRECATED
#'
#' Deprecated in lighthouse 0.7.0 favor of [`max_if_any()`] and [`min_if_any()`], which
#' these now call.
#'
#' @name safe_minmax
#' @export
safe_max <- function(..., na.rm = TRUE) {
  .Deprecated("max_if_any", old = "safe_max")
  max_if_any(..., na.rm = na.rm)
}
#'
#' @rdname safe_minmax
#' @export
safe_min <- function(..., na.rm = TRUE) {
  .Deprecated("min_if_any", old = "safe_min")
  min_if_any(..., na.rm = na.rm)
}

#' Generate NA values of appropriate type - DEPRECATED
#'
#' @description
#' This function was deprecated in lighthouse 0.7.0 because (1) it's always
#' been buggy and (2) its main purpose was to pass appropriate `NA`s to
#' `dplyr::if_else()` and `case_when()`, but this is no longer necessary with
#' \href{https://dplyr.tidyverse.org/news/index.html#vctrs-1-1-0}{changes introduced in dplyr v1.1.0}
#'
#' Returns compatible `NA` based on `x`. This is usually of the same type as `x`
#' (e.g., `NA_real_` if `x` is a double vector). If `x` is a factor, will
#' return `NA_character_` if `factor_as_character = TRUE` (the default) and
#' `NA_integer_` otherwise.
#'
#' @export
na_like <- function(x, factor_as_character = TRUE, match_length = FALSE) {
  .Deprecated(
    msg = "`na_like()` is deprecated. Its main use case no longer applies given newly flexible class handling in `dplyr::if_else()` and `dplyr::case_when()`."
  )
  stopifnot("`x` must be an atomic vector" = is.atomic(x))
  type_out <- if (factor_as_character && is.factor(x)) "character" else typeof(x)
  length_out <- if (match_length) length(x) else 1L
  rep(methods::as(NA, type_out), length_out)
}

#' Median value as double - DEPRECATED
#'
#' @description
#' Deprecated in lighthouse 0.7.0. The main use case for this function was to avoid type errors in `dplyr::if_else()` and `case_when()`, but this is no longer necessary with
#' \href{https://dplyr.tidyverse.org/news/index.html#vctrs-1-1-0}{changes introduced in dplyr v1.1.0}.
#'
#' Returns median of `x` as a double vector. Alternative to `stats::median()`
#' when a consistent return value is needed.
#'
#' @examples
#' \dontrun{
#' # stats::median raises error because of inconsistent return types
#' ### note this no longer raises an error with dplyr >= 1.1.0
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
  .Deprecated(
    msg = "`median_dbl()` is deprecated. Its intended use case no longer applies given newly flexible class handling in `dplyr::if_else()` and `dplyr::case_when()`."
  )
  stopifnot(typeof(x) %in% c("double", "integer", "logical"))
  as.double(stats::median(x, na.rm = na.rm, ...))
}


#' Compute the standard error - DEPRECATED
#'
#' @description
#' Deprecated in 0.7.0 favor of the more specific functions [`se_mean()`] and [`se_mean()`]. `se()` now calls `se_mean()` with a deprecation warning.
#'
#' Computes the standard error of the values in x.
#'
#' @param x a numeric vector or a non-factor object coercible to numeric by `as.double(x)`.
#' @param na.rm	logical. Should missing values be removed?
#'
#' @export
se <- function(x, na.rm = FALSE) {
  .Deprecated(msg = "`se()` is deprecated. Use `se_mean()` or `se_prop()` instead.")
  se_mean(x, na.rm)
}


se <- function(x, na.rm = FALSE) {
  .Deprecated(msg = "`se()` is deprecated. Use `se_mean()` or `se_prop()` instead.")
  se_mean(x, na.rm)
}

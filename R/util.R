#' Nicer ggplot rendering
#'
#' Saves ggplot object as .svg in R temp directory, then displays in RStudio
#' Viewer pane. Results in better image quality on Windows machines.
#'
#' @export
ggview <- function(plot = ggplot2::last_plot(),
                   width = NULL,
                   height = NULL,
                   type = c("svg", "png")) {
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

#' Open dataframe in Excel
#'
#' Saves dataframe as .csv in R temp directory, then opens in Excel. The .csv
#' will have a randomly-generated name unless otherwise specified in `name`.
#'
#' @export
in_excel <- function (df, name, na = "") {
  csv_dir <- file.path(tempdir(), "csv")
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir)
  }
  if (missing(name)) {
    csv_path <- tempfile(tmpdir = csv_dir, fileext = ".csv")
  } else {
    csv_path <- file.path(csv_dir, paste0(name, ".csv"))
  }
  readr::write_excel_csv(df, csv_path, na = na)
  shell.exec(csv_path)
}


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
  withCallingHandlers(
    warning = function(w) {
      if (conditionMessage(w) == "NAs introduced by coercion") {
        rlang::cnd_muffle(w)
      }
    },
    as.numeric(x)
  )
}

#' @rdname try_numeric
#'
#' @export
try.numeric <- function(x) try_numeric(x)


#' Generate NA values of appropriate type
#'
#' Returns compatible `NA` based on `x`. This is usually of the same type as `x`
#' (e.g., `NA_real_` if `x` is a double vector). If `x` is a factor, will
#' return `NA_character_` if `factor_as_character = TRUE` (the default) and
#' `NA_integer_` otherwise.
#'
#' @export
na_like <- function(x, factor_as_character = TRUE, match_length = FALSE) {
  stopifnot("`x` must be an atomic vector" = is.atomic(x))
  type_out <- if (factor_as_character && is.factor(x)) "character" else typeof(x)
  length_out <- if (match_length) length(x) else 1L
  rep(methods::as(NA, type_out), length_out)
}


#' Set NA values based on numbers stored as strings.
#'
#' Changes values coercible to numeric in range `range_min`:`range_max` to `NA`.
#' Useful for imported SPSS files.
#'
#' @export
coerce_na_range <- function(x, ...) UseMethod("coerce_na_range")
coerce_na_range.default <- function(x, range_min = -Inf, range_max = -1) {
  coerced <- try.numeric(x)
  dplyr::if_else(
    is.na(coerced) | (coerced < range_min) | (coerced > range_max),
    x,
    na_like(x)
  )
}
coerce_na_range.factor <- function(x, range_min = -Inf, range_max = -1) {
  lvls <- levels(x)
  coerced <- try.numeric(as.character(lvls))
  lvls <- lvls[is.na(coerced) | coerced < range_min | coerced > range_max]
  factor(x, levels = lvls)
}


#' Summarize a dataframe's column types - DEPRECATED
#'
#' Returns class and type for each column in `.data`. Deprecated in favor of
#' `cols_info()`, which provides more information and features and is more
#' stable.
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

#' Count observations with total
#'
#' Adds row with column totals to results of [`dplyr::count()`][count]. Can take
#' multiple variables, but totals only by first variable given.
#'
#' @export
count_with_total <- function(.data,
                             ...,
                             totals_for = NULL,
                             label = "Total",
                             first_row = FALSE,
                             .drop = TRUE) {
  vars <- rlang::enquos(...)
  counts <- dplyr::count(.data, ..., .drop = .drop)
  if (missing(totals_for)) {
    totals <- .data %>%
      dplyr::mutate(!!vars[[1]] := label) %>%
      dplyr::count(..., .drop = .drop)
    if (first_row) {
      dplyr::bind_rows(totals, counts)
    } else {
      dplyr::bind_rows(counts, totals)
    }
  } else {
    totals <- .data %>%
      dplyr::mutate({{totals_for}} := label) %>%
      dplyr::count(..., .drop = .drop)
    if (first_row) {
      out <- dplyr::bind_rows(totals, counts)
    } else {
      out <- dplyr::bind_rows(counts, totals)
    }
    dplyr::arrange(
      out,
      !!!(vars[!purrr::map_lgl(vars, identical, rlang::enquo(totals_for))])
    )
  }
}

#' Count observations with percentage
#'
#' Adds column with percent of total to results of [`dplyr::count()`][count].
#'
#' @export
count_pct <- function(.data, ..., na.rm = FALSE, multiply = FALSE) {
  cnt <- dplyr::count(.data, ...)
  if (na.rm) cnt <- tidyr::drop_na(cnt)
  if (multiply) {
    dplyr::mutate(cnt, pct = 100 * (n / sum(n)))
  } else {
    dplyr::mutate(cnt, pct = n / sum(n))
  }
}

#' Count observations for multiple variables
#'
#' Returns a table with frequencies for each column passed to `...`.
#'
#' @export
count_multiple <- function(.data,
                           ...,
                           .pct = TRUE,
                           sort = FALSE,
                           name = NULL,
                           na.rm = FALSE,
                           .drop = TRUE) {
  name <- name %||% "n"
  args <- rlang::enexprs(...)
  if (!is.null(names(args))) {
    is_computed <- purrr::map_lgl(names(args), ~ stringr::str_length(.x) > 0)
    names(args)[!is_computed] <- args[!is_computed]
    .data <- dplyr::mutate(.data, !!!args)
    args <- rlang::syms(names(args))
  }
  grps <- dplyr::groups(.data)
  out <- purrr::map_dfr(
    args,
    ~ .data %>%
      dplyr::count(
        !!.x,
        sort = sort,
        name = name,
        .drop = .drop
      ) %>%
      tidyr::pivot_longer(
        cols = !c(all_of(name), !!!grps),
        names_to = "Variable",
        values_to = "Value"
      ) %>%
      dplyr::mutate(Value = forcats::fct_inorder(as.character(Value)))
    # dplyr::mutate(Value = factor(Value))
  )
  if(na.rm) out <- tidyr::drop_na(out)
  if (.pct) {
    out <- out %>%
      dplyr::group_by(!!!grps, Variable) %>%
      dplyr::mutate(pct = n / sum(n)) %>%
      dplyr::group_by(!!!grps)
  }
  dplyr::relocate(out, !!!grps, Variable, Value, n)
}

#' Count duplicates
#'
#' @export
count_duplicates <- function(.data, ..., na.rm = FALSE) {
  if(na.rm) .data <- tidyr::drop_na(.data, ...)
  .data %>%
    dplyr::count(..., name = "instances") %>%
    dplyr::count(instances)
}


#' Count number of unique values in a set of dataframe columns
#'
#' A pipe-friendly wrapper around `dplyr::n_distinct()`.
#'
#' @export
count_unique <- function(.data, ..., name = "n_unique", na.rm = FALSE) {
  .data %>%
    dplyr::summarize(
      !!name := dplyr::n_distinct(!!!rlang::enquos(...), na.rm = na.rm)
    )
}

#' Identify duplicates within a vector or set of vectors.
#'
#' @examples
#' x <- c(1, 2, 2, 3, 3, 3)
#' y <- c(1, 1, 2, 1, 2, 2)
#' is_duplicate(x)
#' is_duplicate(x, y)
#'
#' @export
is_duplicate <- function(..., nmax = 1, incomparables = FALSE) {
  if (length(unique(purrr::map_int(list(...), length))) != 1) {
    stop("All vectors passed to `...` must be of equal length.")
  }
  withCallingHandlers(
    x <- tibble::tibble(..., .name_repair = "unique"),
    message = function(msg) {
      if (stringr::str_starts(msg$message, "New names:")) rlang::cnd_muffle(msg)
    }
  )
  x <- dplyr::add_count(x, dplyr::across(tidyselect::everything()))
  if (!incomparables) {
    x <- x %>%
      dplyr::mutate(
        is_dup = !dplyr::if_any(!n, ~ is.na(.x) | is.nan(.x)) & n > nmax
      )
  } else {
    x <- dplyr::mutate(x, is_dup = n > nmax)
  }
  x$is_dup
}


#' Add empty rows
#'
#' Adds the number of empty rows passed to `.nrows` (default 1) at positions
#' passed to `.before` or `.after`. Vectorized over `.before`, `.after`, and
#' `.nrows`.
#'
#' @export
add_empty_rows <- function(.data, .before = NULL, .after = NULL, .nrows = 1) {
  if (!is.null(.before) && !is.null(.after)) {
    stop("Can't specify both `.before` and `.after`.")
  }
  if (is.null(.after)) .after <- .before - 1
  sort_order <- order(.after, decreasing = TRUE)
  .after <- .after[sort_order]
  if (length(.nrows) != 1 && length(.nrows) != length(.after)) {
    stop("`.nrows` must be either length 1 or same length as `.before` or `.after`.")
  }
  if (length(.nrows) == 1) {
    .rows <- tibble::tibble(.rows = .nrows)
    for (.a in .after) {
      .data <- tibble::add_row(.data, .rows, .after = .a)
    }
  } else {
    .nrows = .nrows[sort_order]
    for (i in seq_along(.after)) {
      .rows <- tibble::tibble(.rows = .nrows[[i]])
      .data <- tibble::add_row(.data, .rows, .after = .after[[i]])
    }
  }
  .data
}

#' Collapse a character vector into a single string
#'
#' Concatenates all elements in a character vector. Essentially a column-wise
#' variant of dplyr::str_c(). Can accept multiple character vectors, which are
#' returned as separate character strings if `join` = NULL (the default), or
#' concatenated with a separator supplied to `join`.
#'
#' @export
str_collapse <- function(..., sep = "", join = NULL) {
  out <- purrr::map_chr(list(...), ~ stringr::str_c(.x, collapse = sep))
  if (!is.null(join)) str_collapse(out, sep = join) else out
}

#' Detect the presence of any pattern in a string
#'
#' Returns TRUE if any element of `patterns` is present in `string`. Matches
#' whole words if `whole_word` = TRUE (FALSE by default). Case sensitive unless
#' `ignore_case` = TRUE (FALSE by default).
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

#' Compute the standard error
#'
#' Computes the standard error of the values in x.
#'
#' @param x a numeric vector or an R object but not a `factor` coercible to
#' numeric by `as.double(x)`.
#'
#' @param na.rm	logical. Should missing values be removed?
#'
#' @export
se <- function(x, na.rm = FALSE) {
  stats::sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
}

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

#' Compute odds ratio from probabilities
#'
#' Given probabilities `p1` and `p2`, returns the odds ratio (odds of `p2`
#' relative to odds of `p1`).
#'
#' @export
p_to_OR <- function(p1, p2) (p2 / (1 - p2)) / (p1 / (1 - p1))

#' Conversion between Cohen's d and odds ratio
#'
#' Functions to convert Cohen's d to an odds ratio and vice versa.
#'
#' @export
d_to_OR <- function(d) exp((d * pi) / sqrt(3))

#' @rdname d_to_OR
#'
#' @export
OR_to_d <- function(OR) log(OR) * (sqrt(3) / pi)

#' Compute Cohen's kappa and weighted kappa
#'
#' A tidyverse-friendly wrapper around `psych::cohen.kappa()`.
#' @export
wkappa <- function(.data, x, y) {
  if (!requireNamespace("broom", quietly = TRUE) |
      !requireNamespace("psych", quietly = TRUE)) {
    stop('"psych" and "broom" packages must be installed to use `wkappa()`.')
  }
  .data %>%
    dplyr::select({{x}}, {{y}}) %>%
    as.data.frame() %>%
    psych::cohen.kappa() %>%
    broom::tidy()
}

#'  Reverse the order of rows in a table.
#'
#' @export
rev_rows <- function(.data) dplyr::arrange(.data, rev(dplyr::row_number()))

#' Descending cumulative sum
#'
#' Returns a cumulative sum beginning with the last element of `x`.
#'
#' @examples
#' ggplot2::diamonds %>%
#'   dplyr::count(cut) %>%
#'   dplyr::mutate(
#'     or_worse = cumsum(n),
#'     or_better = cumsum_desc(n)
#'   )
#'
#' @export
cumsum_desc <- function(x) rev(cumsum(rev(x)))

#' Reverse key a numeric vector
#'
#' Reverses a numeric vector `x` by subtracting from `min` and adding `max`.
#' Observed minimum and maximum of `x` are used unless otherwise specified.
#'
#' @examples
#' reverse_key(1:5)
#' reverse_key(3:5)
#' reverse_key(3:5, min = 1, max = 5)
#'
#' @export
reverse_key <- function(x,
                    na.rm = FALSE,
                    max = NULL,
                    min = NULL) {
  if (is.null(max)) max <- max(x, na.rm = na.rm)
  if (is.null(min)) min <- min(x, na.rm = na.rm)
  max - x + min
}

#' Convert all datetimes in table to dates
#'
#' Returns a dataframe with all datetime columns (i.e., class `POSIXct` or
#' `POSIXlt`) converted to `Date`.
#'
#' @export
datetimes_to_date <- function(.data) {
  .data %>%
    dplyr::mutate(
      dplyr::across(where(lubridate::is.POSIXt), lubridate::as_date)
    )
}

#' Test whether a dataframe contains SPSS variable or value labels.
#'
#' @export
is_spss <- function(.data) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop('"labelled" package must be installed to use `is_spss()`.')
  }
  purrr::some(
    .data,
    ~ !is.null(labelled::var_label(.x)) | labelled::is.labelled(.x) |
      "format.spss" %in% names(attributes(.x))
  )
}


#' Get information about dataframe columns
#'
#' Returns a summary of each column's class, type, and missing data. If
#' dataframe was imported from SPSS .sav and "labelled" package is installed,
#' SPSS variable labels will also be included.
#'
#' @export
cols_info <- function(x, zap_spss = TRUE) {
  if (requireNamespace("labelled", quietly = TRUE)) {
    spss_data <- is_spss(x)
    if (spss_data) {
      var_labels <- labelled::var_label(x) %>%
        unlist()
      if (zap_spss) x <- zap_everything(x)
    }
  } else {
    spss_data <- FALSE
    if (purrr::some(x, ~ "format.spss" %in% names(attributes(.x)))) {
      message('Install "labelled" package for additional functionality for SPSS dataframes.')
      if (zap_spss) warning('`zap_spss` argument has no effect because "labelled" package is not installed.')
    }
  }
  out <- tibble::tibble(
    column = names(x),
    class = purrr::map_chr(x, ~ str_collapse(class(.x), sep = ", ")),
    type = purrr::map_chr(x, typeof),
    valid_n = purrr::map_dbl(x, n_valid),
    valid_pct = valid_n / nrow(x)
  )
  if (spss_data) {
    out <- out %>%
      dplyr::mutate(label = var_labels[column])
  }
  out
}


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
#' Returns a vector with `NA`s removed.
#'
#' @export
discard_na <- function(x) x[!is.na(x)]


#' Generate random logicals - DEPRECATED
#'
#' Returns a vector of random logicals of length `n`, drawn from binomial
#' distribution with trial probability `prob` (default = .5). Deprecated once
#' Casey realized [`purrr::rbernoulli()`][rbernoulli] exists and does exactly
#' this.
#'
#' @export
rbool <- function(n, prob = .5) {
  .Deprecated("purrr::rbernoulli", old = "rbool")
  as.logical(stats::rbinom(n, 1, prob))
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

#' Strip special attributes from SPSS dataset
#'
#' Removes all special attributes from data read from SPSS, optionally
#' converting labelled vectors to factors if `.as_factor` = TRUE (the default).
#'
#' @export
zap_everything <- function(.data, ..., .as_factor = TRUE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop('"haven" package must be installed to use `zap_everything()`.')
  }
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

#' Find the next business day
#'
#' Returns the next date that isn't a weekend or holiday.
#'
#' @param x A `Date` or coercible to `Date`
#'
#' @param include_today If `x` is a business day, will return `x` if `TRUE` and
#' the next business day after `x` otherwise.
#'
#' @param holidays Holiday calendar to use. Available options are "Chestnut" for
#' Chestnut Health System holidays, "Illinois" for State of Illinois holidays,
#' and "federal" for United States federal holidays.
#'
#' @export
next_bizday <- function(x,
                        include_today = TRUE,
                        holidays = c("Chestnut", "Illinois", "federal")) {
  bizdays <- business_days[[match.arg(holidays)]]
  inrange <- dplyr::between(x, attr(bizdays, "start"), attr(bizdays, "end"))
  if (!all(is_TRUE_or_NA(inrange))) stop(
    "Dates before ", attr(bizdays, "start"),
    " or after ", attr(bizdays, "end"),
    " are not currently supported."
  )
  if (!include_today) x <- x + lubridate::days(1)
  purrr::map(
    lubridate::as_date(x),
    ~ bizdays[bizdays >= .x][[1]]
  ) %>%
    do.call(c, .)
}

#' Reorder a dendrogram
#'
#' Reorders dendrogram leaves based on the vector of labels passed to `.order`.
#'
#' @export
reorder_dendro_by_label <- function(.dendro, .order, agglo.FUN = max) {
  current <- stats::setNames(order(.dendro), labels(.dendro))
  new <- rep(0, length(current))
  wt <- length(current)
  for (.val in .order) {
    ind <- current[[.val]]
    new[[ind]] <- wt
    wt <- wt - 1
  }
  stats::reorder(.dendro, new, agglo.FUN = agglo.FUN)
}

#' Replicate SPSS SUM() function
#'
#' Sums across columns a la SPSS: `NA`s are counted as 0s, but if ALL variables
#' are `NA`, then result is `NA`.
#'
#' @export
row_sums_spss <- function(...) {
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


#' Add header rows to a table
#'
#' Inserts header rows in `to` using unique values of `from`.
#'
#' @examples
#' dplyr::starwars %>%
#'   head(13) %>%
#'   dplyr::arrange(species) %>%
#'   add_header(from = species, to = name, indent = "  ")
#'
#' @export
add_header <- function(data,
                       from,
                       to,
                       skip_single_row = FALSE,
                       indent = "",
                       drop_from = TRUE) {
  data <- data %>%
    dplyr::add_count({{ from }}, name = "..SKIP") %>%
    dplyr::mutate(
      ..FROM = {{ from }},
      ..SKIP = {if (skip_single_row) ..SKIP == 1 else FALSE},
      ..SKIP = ..SKIP | is.na({{ from }}),
      ..NEW_HEADER = !is_TRUE(..FROM == dplyr::lag(..FROM)) & !..SKIP,
      !!rlang::ensym(to) := dplyr::if_else(
        ..SKIP,
        {{ to }},
        stringr::str_c(indent, {{ to }})
      )
    )
  for (loc in rev(which(data$..NEW_HEADER))) {
    data <- dplyr::add_row(
      data,
      !!rlang::ensym(to) := data$..FROM[[loc]],
      .before = loc
    )
  }
  if (drop_from) data <- dplyr::select(data, !{{ from }})
  dplyr::select(data, !..SKIP:..NEW_HEADER)
}

#' Split dataframe by named groups
#'
#' Divides `.data` into a named list of dataframes defined by the grouping
#' structure. Grouping variables can optionally be passed to `...`. A nested
#' list will be returned if there is more than one grouping variable and
#' `.nested` = `TRUE`.
#'
#' @examples
#' by_cyl_gear1 <- mtcars %>%
#'   group_split_named(cyl, gear, .col_names = TRUE)
#' by_cyl_gear1$cyl_6.gear_4
#'
#' by_cyl_gear2 <- mtcars %>%
#'   group_split_named(cyl, gear, .col_names = TRUE, .nested = TRUE)
#' by_cyl_gear2$cyl_6
#' by_cyl_gear2$cyl_6$gear_4
#'
#' @export
group_split_named <- function(.data,
                              ...,
                              .keep = TRUE,
                              .sep = ".",
                              .col_names = FALSE,
                              .col_sep = "_",
                              .nested = FALSE,
                              .na.rm = FALSE,
                              .add_groups = TRUE) {
  .data <- dplyr::group_by(.data, ..., .add = .add_groups)
  df_groups <- dplyr::groups(.data)
  if (length(df_groups) == 0) {
    list(.data)
  } else if (length(df_groups) == 1 || !.nested) {
    .data <- dplyr::group_nest(.data, .key = "..DATA..", keep = .keep)
    if (.na.rm) .data <- tidyr::drop_na(.data)
    .names <- dplyr::select(.data, !..DATA..)
    if (.col_names) {
      .names <- purrr::imap(.names, ~ paste(.y, .x, sep = .col_sep))
    } else {
      .names <- purrr::map(.names, ~ tidyr::replace_na(as.character(.x), "NA"))
    }
    .names <- interaction(.names, sep = .sep)
    stats::setNames(as.list(.data[["..DATA.."]]), .names)
  } else {
    .data %>%
      group_split_named(
        !!df_groups[[1]],
        .keep = .keep,
        .col_names = .col_names,
        .col_sep = .col_sep,
        .na.rm = .na.rm,
        .add_groups = FALSE
      ) %>%
      purrr::map(
        ~ group_split_named(
          .x,
          !!!df_groups[-1],
          .keep = .keep,
          .col_names = .col_names,
          .col_sep = .col_sep,
          .nested = TRUE,
          .na.rm = .na.rm,
          .add_groups = FALSE
        )
      )
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

#' Alternative column ordering and naming for pivot_wider()
#'
#' A wrapper around [`tidyr::pivot_wider()`][pivot_wider] with additional
#' options for sorting and naming of output columns, with arguments
#' `sort_by_col`, `names_value_first`, and `names_value_sep` These options are
#' only relevant when more than one input column is passed to `values_from`.
#'
#' The `names_value_sep` argument allows output column names that use a
#' different separator between `{.value}` and `{column}` than between multiple
#' `{columns}`s. Example:
#' ```
#' pivot_wider_alt(
#'   fakedata,
#'   names_from = c(size, color), # size = "sm", "med", "lg"; color = "red", "blue"
#'   values_from = c(n, weight),
#'   names_sep = "_",
#'   names_value_sep = ": "
#' )
#'
#' # output column names:
#' # `n: sm_red`, `weight: sm_red`, `n: sm_blue`, `weight: sm_blue`, `n: med_red`...
#' ```
#'
#' @inheritParams tidyr::pivot_wider
#' @param sort_by_col If `TRUE` (default), output columns will be sorted by
#' `names_from`, then `values_from`. (Differs from `tidyr::pivot_wider()`,
#' which sorts by `values_from` first, then `names_from`.)
#' @param names_value_first If `FALSE`, output columns will be named using a
#' `{column}_{.value}` scheme. (Differs from `tidyr::pivot_wider()`, which uses
#' a `{.value}_{column}` scheme.)
#' @param names_value_sep,names_sep If `names_from` or `values_from` contain
#' multiple variables, these will be used to join their values together into a
#' single string to use as a column name. `names_value_sep` will separate
#' `{.value}` from `{column}` components, while `names_sep` will separate
#' `{column}` components from one another if `names_from` contains multiple
#' variables. See Details and Examples.
#' @param names_prefix,names_glue,names_repair,values_from,values_fill,values_fn
#' See documentation at [`tidyr::pivot_wider()`][pivot_wider].
#'
#' @examples
#' data_ex <- ggplot2::diamonds %>%
#'   dplyr::group_by(cut, color) %>%
#'   dplyr::summarize(Min = min(price), Median = median(price), Max = max(price))
#'
#' # default pivot_wider() behavior
#' data_ex %>%
#'   tidyr::pivot_wider(
#'     id_cols = color,
#'     names_from = cut,
#'     values_from = Min:Max
#'   )
#'
#' # pivot_wider_alt() behavior
#' data_ex %>%
#'   pivot_wider_alt(
#'     id_cols = color,
#'     names_from = cut,
#'     values_from = Min:Max
#' )
#'
#' # with `names_value_first` = FALSE
#' data_ex %>%
#'   pivot_wider_alt(
#'     id_cols = color,
#'     names_from = cut,
#'     values_from = Min:Max,
#'     names_value_first = FALSE
#'   )
#'
#' # multiple `names_from` vars, with different value vs. name separators
#' ggplot2::mpg %>%
#'   dplyr::filter(class %in% c("compact", "subcompact", "midsize")) %>%
#'   dplyr::group_by(
#'     manufacturer,
#'     trans = stringr::str_extract(trans, ".*(?=\\()"), year
#'   ) %>%
#'   dplyr::summarize(across(c(cty, hwy), mean)) %>%
#'   pivot_wider_alt(
#'     names_from = trans:year,
#'     values_from = cty:hwy,
#'     names_sep = "_",
#'     names_value_sep = ": "
#'   )
#'
#' @export
pivot_wider_alt <- function(data,
                            id_cols = NULL,
                            names_from = name,
                            sort_by_col = TRUE,
                            names_value_first = TRUE,
                            names_value_sep = ".",
                            names_sep = "_",
                            names_prefix = "",
                            names_glue = NULL,
                            names_repair = "check_unique",
                            values_from = value,
                            values_fill = NULL,
                            values_fn = NULL) {
  # lighthouse 0.6.0:
  # fixed bug by naming args to `pivot_wider_spec`
  # with tidyr 1.2.0, `pivot_wider` now has more options for column sorting etc -
  # may want to use those and simplify this Fx
  names_from_untidy <- untidyselect(data, {{ names_from }}, syms = TRUE)
  pivot_spec <- tidyr::build_wider_spec(
    data,
    {{ names_from }},
    {{ values_from }},
    names_prefix,
    names_sep,
    names_glue
  )
  if (sort_by_col) {
    pivot_spec <- dplyr::arrange(pivot_spec, !!!names_from_untidy)
  }
  if (missing(names_glue)) {
    if (names_value_first) {
      pivot_spec <- dplyr::mutate(pivot_spec, .name = stringr::str_c(
        .value,
        names_value_sep,
        stringr::str_c(
          names_prefix,
          stringr::str_c(!!!names_from_untidy, sep = names_sep)
        )
      ))
    } else {
      pivot_spec <- dplyr::mutate(pivot_spec, .name = stringr::str_c(
        stringr::str_c(
          names_prefix,
          stringr::str_c(!!!names_from_untidy, sep = names_sep)
        ),
        names_value_sep,
        .value
      ))
    }
  }
  tidyr::pivot_wider_spec(
    data,
    spec = pivot_spec,
    names_repair = names_repair,
    id_cols = {{ id_cols }},
    values_fill = values_fill,
    values_fn = values_fn
  )
}

#' Custom summary table
#'
#' Generates a summary table, with a row for each variable passed to `.vars` and
#' a column for each function passed to `...`.
#'
#' @param .data A data frame or data frame extension (e.g. a tibble).
#'
#' @param ... Functions to apply to each variable specified in `.vars`, as
#' function names (e.g., `mean`) or purrr-style lambdas (e.g.,
#' `~ mean(.x, na.rm = TRUE)`). Each function passed to `...` will yield one
#' column in the output table (or one column per group if `.cols_group_by` is
#' specified). Output column names can optionally be specified. E.g.,
#' `m = mean, sd, sem = ~ sd(.x) / sqrt(n())` yields output columns named `m`,
#' `sd`, and `sem`.
#'
#' @param .vars <[`tidy-select`][dplyr_tidy_select]> Columns in `.data` to
#' summarize. Each column passed to `.vars` will yield one row in the output
#' table (or one row per group if `.rows_group_by` is specified).
#'
#' @param na.rm A logical value passed to any functions in `...` that take an
#' `na.rm` argument.
#'
#' @param .rows_group_by <[`tidy-select`][dplyr_tidy_select]> Grouping
#' variable(s) for output rows.
#'
#' @param .cols_group_by <[`tidy-select`][dplyr_tidy_select]> Grouping
#' variable(s) for output columns.
#'
#' @param .cols_group_opts A list of arguments passed to
#' [`lighthouse::pivot_wider_alt()`][pivot_wider_alt] if `.cols_group_by` is
#' specified.
#'
#' @param .var_col_name The name of the output column containing variable names
#' passed to `.vars`.
#'
#' @examples
#' # example data
#' mtcars2 <- mtcars %>%
#'   dplyr::mutate(
#'     Transmission = dplyr::recode(am, `0` = "auto", `1` = "manual")
#'   )
#'
#' # simple summary table. note specification of column and row names
#' # for "n", "m", and "weight".
#' mtcars2 %>%
#'   summary_table(
#'     n = ~ sum(!is.na(.x)), m = mean, sd,
#'     .vars = c(mpg, hp, weight = wt)
#'   )
#'
#' # with column and row groupings
#' mtcars2 %>%
#'   summary_table(
#'     n = ~ sum(!is.na(.x)), m = mean, sd,
#'     .vars = c(mpg, hp, weight = wt),
#'     .cols_group_by = cyl,
#'     .rows_group_by = Transmission
#'   )
#'
#' # customize output column names with `.cols_group_opts`
#' mtcars2 %>%
#'   summary_table(
#'     n = ~ sum(!is.na(.x)), m = mean, sd,
#'     .vars = c(mpg, hp, weight = wt),
#'     .cols_group_by = cyl,
#'     .rows_group_by = Transmission,
#'     .cols_group_opts = list(
#'       names_value_first = FALSE,
#'       names_prefix = "cyl",
#'       names_value_sep = ": "
#'     )
#'   )
#'
#' # you can alternatively pass a glue specification to `.cols_group_opts`
#' mtcars2 %>%
#'   summary_table(
#'     n = ~ sum(!is.na(.x)), m = mean, sd,
#'     .vars = c(mpg, hp, weight = wt),
#'     .cols_group_by = cyl,
#'     .rows_group_by = Transmission,
#'     .cols_group_opts = list(names_glue = "{.value}: {cyl}-cyl")
#'   )
#'
#' @export
summary_table <- function(.data,
                          ...,
                          .vars = where(is.numeric),
                          na.rm = FALSE,
                          .rows_group_by = NULL,
                          .cols_group_by = NULL,
                          .cols_group_opts = list(),
                          .var_col_name = "Variable") {
  .fns <- named_fn_list(...)
  .data <- dplyr::group_by(
    .data,
    {{ .rows_group_by }},
    {{ .cols_group_by }},
    .add = TRUE
  )
  .group_vars <- dplyr::group_vars(.data)
  .data <- .data %>%
    dplyr::summarize(
      dplyr::across(
        .cols = {{ .vars }},
        .fns = .fns,
        na.rm = na.rm,
        .names = "{.col}__SEP__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = !tidyselect::all_of(.group_vars),
      names_to = c(.var_col_name, ".value"),
      names_sep = "__SEP__"
    )
  if (!missing(.cols_group_by)) {
    rlang::exec(
      pivot_wider_alt,
      .data,
      names_from = rlang::enexpr(.cols_group_by),
      values_from = tidyselect::all_of(names(.fns)),
      !!!.cols_group_opts
    )
  } else {
    .data
  }
}

#' Compute common accuracy and agreement metrics
#'
#' Given a vector of `true_values` and one or more vectors of test values
#' (passed to `...`), computes sensitivity, specificity, positive predictive
#' value (PPV), negative predictive value (NPV), and Cohen's kappa.
#'
#' @examples
#' # create example data w predictors with different properties:
#' ex_data <- tibble::tibble(
#'   actual = rbinom(250, 1, .3),                                   # 250 cases, p(outcome) = .3
#'   prediction1 = ifelse(runif(250) <= .05, 1L - actual, actual), # 5% error rate
#'   prediction2 = ifelse(runif(250) <= .15, 1L - actual, actual), # 15% error rate
#'   prediction3 = ifelse(runif(250) <= .35, 1L - actual, actual), # 35% error rate
#'   prediction4 = ifelse(runif(250) <= .15, 1L, actual),          # 15% with positive bias
#'   prediction5 = ifelse(runif(250) <= .15, 0L, actual)           # 15% with negative bias
#' )
#'
#' # testing predicted v actual values
#' ex_data %>%
#'   accuracy_stats(actual, prediction1)
#'
#' # can test multiple predictors simultaneously
#' ex_data %>%
#'   accuracy_stats(actual, prediction1:prediction5)
#'
#' # if `include_counts` = TRUE, will also return n of false positives,
#' # false negatives, etc., as well as and observed and expected % agreement
#' ex_data %>%
#'   accuracy_stats(actual, prediction1:prediction5, include_counts = TRUE)
#'
#' @export
accuracy_stats <- function(.data, true_values, ..., include_counts = FALSE) {
  .data <- .data %>%
    dplyr::select(true_values = {{true_values}}, ...)
  pred_names <- names(dplyr::select(.data, !true_values))
  out <- .data %>%
    tidyr::pivot_longer(!true_values, names_to = "Predictor") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      Predictor = factor(Predictor, levels = pred_names),
      result = fct_case_when(
        value & true_values ~ "TP",
        value & !true_values ~ "FP",
        !value & !true_values ~ "TN",
        !value & true_values ~ "FN"
      )
    ) %>%
    dplyr::count(Predictor, result, .drop = FALSE) %>%
    tidyr::pivot_wider(
      id_cols = Predictor,
      names_from = result,
      values_from = n
    ) %>%
    dplyr::mutate(
      n = rowSums(dplyr::across(TP:FN)),
      pAgreeObserved = (TP + TN) / n,
      pAgreeExpected = (((TP + FN) * (TP + FP)) + ((FP + TN) * (FN + TN))) / (n^2),
      Kappa = (pAgreeObserved - pAgreeExpected) / (1 - pAgreeExpected),
      Sensitivity = TP / (TP + FN),
      Specificity = TN / (TN + FP),
      PPV = TP / (TP + FP),
      NPV = TN / (TN + FN)
    )
  if (include_counts) {
    dplyr::relocate(out, Predictor, n)
  } else {
    dplyr::select(out, !c(TP:FN, pAgreeObserved:pAgreeExpected))
  }
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


#' Print specified number of tibble rows
#'
#' @export
print_n <- function(x, n, ...) print(x, n = n, ...)

#' Print all tibble rows
#'
#' Actually limits printing to `RStudioPreference` `"console_max_lines"` (or
#' 1000 lines if not running in RStudio) unless otherwise specified in `max`.
#' Works only with `tibble`s, not `base::data.frame`s.
#'
#' @export
print_all <- function(x, ..., max = NULL) {
  if (is.null(max)) {
    if (Sys.getenv("RSTUDIO") == 1) {
      max <- rstudioapi::readRStudioPreference(
        "console_max_lines",
        default = 1000
      )
    } else {
      max <- 1000
    }
  }
  print(x, n = max, ...)
}

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

is_logical_vec <- function(x) {
  stopifnot(rlang::is_vector(x))
  if (rlang::is_list(x)) {
    rep(is.logical(x), length(x))
  } else {
    purrr::map_lgl(x, is.logical)
  }
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


#' Return asterisks corresponding to p-values
#'
#' @export
asterisks <- function(p,
                      trends = TRUE,
                      levels = c(.1, .05, .01, .001),
                      marks = c(sig = "*", trend = "+", ns = NA_character_),
                      include_key = TRUE) {
  if (any(!is.na(p) & !dplyr::between(p, 0, 1))) {
    stop("p values <0 or >1 detected. Check your input.")
  }
  if (!missing(marks)) {
    defaults <- c(sig = "*", trend = "+", ns = NA_character_)
    if (any(names(marks) %!in% names(defaults))) {
      warning("Unrecognized name in `marks`, ignored.")
    }
    for (nm in names(marks)) {
      defaults[[nm]] <- marks[[nm]]
    }
    marks <- defaults
  }
  purrr::iwalk(marks, ~ assign(.y, .x, pos = 1))
  if (!trends) {
    if(missing(levels)) levels <- levels[2:4]
    names(levels) <- strrep(sig, seq_along(levels))
  } else {
    names(levels) <- strrep(sig, seq_along(levels) - 1)
    names(levels)[[1]] <- trend
  }
  levels <- rev(levels)
  if (is.na(ns)) {
    breaks <- c(-Inf, levels)
    labels <- names(levels)
  } else {
    breaks <- c(-Inf, levels, Inf)
    labels <- c(names(levels), ns)
  }
  out <- cut(p, breaks = breaks, labels = labels, right = FALSE)
  if (include_key) attr(out, "key") <- levels
  out
}

#' Scale based on median absolute deviation
#'
#' Scales a vector of values based on the median absolute deviation. Values may
#' be centered around the median (default), mean, or not centered. Compare to
#' `base::scale()`, which uses standard deviation and centers around the mean by
#' default.
#'
#' @export
scale_mad <- function(x,
                      center = c("median", "mean", "none"),
                      mad_constant = 1.4826) {
  center <- match.arg(center)
  ctr <- switch(
    center,
    median = stats::median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    none = 0
  )
  (x - ctr) / stats::mad(x, center = ctr, constant = mad_constant, na.rm = TRUE)
}

#' Winsorize extreme values
#'
#' Sets all values more than `max_dev` deviations from center to be `max_dev`
#' deviations from center. Deviations defined as standard deviation (the
#' default) or mean absolute deviation (if `method = "mad"`). Center defined as
#' mean for `method = "sd"` and median for `method = "mad"`, unless otherwise
#' specified in `center` argument.
#'
#' @export
winsorize <- function(x,
                      max_dev = 3,
                      method = c("sd", "mad"),
                      mad.center = c("median", "mean")) {
  method <- match.arg(method)
  if (method == "sd" && !missing(mad.center)) {
    warning('Argument `mad.center` ignored when `method` = "sd"')
  }
  if (method == "sd") {
    cent <- mean(x, na.rm = TRUE)
    dev <- stats::sd(x, na.rm = TRUE)
  } else {
    mad.center <- match.arg(mad.center)
    cent <- switch(
      mad.center,
      median = stats::median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE)
    )
    dev <- stats::mad(x, center = cent, na.rm = TRUE)
  }
  xmin <- cent - (max_dev * dev)
  xmax <- cent + (max_dev * dev)
  pmax(pmin(x, xmax), xmin)
}

#' Floor methods for date objects
#'
#' @description
#' `floor_month()` and `floor_week()` are simple wrappers around
#' `lubridate::floor_date()` that round down to the first day of the month or
#' week.
#'
#' `floor_days()` rounds down to the nearest `n`-day increment. Floors are
#' defined relative to the earliest date in `x`, unless a different start date
#' is passed to `start`. Default behavior differs from
#' `lubridate::floor_date(x, unit = "{n} days")`, which "resets" the floor to
#' the first of the month with each month. `lubridate`-like behavior can be
#' achieved by setting `reset_monthly = TRUE`.
#'
#' @export
floor_month <- function(x) lubridate::floor_date(x, unit = "month")

#' @rdname floor_month
#'
#' @export
floor_week <- function(x, week_start = getOption("lubridate.week.start", 7)) {
  lubridate::floor_date(x, unit = "week", week_start = week_start)
}

#' @rdname floor_month
#'
#' @export
floor_days <- function(x,
                       n = 1L,
                       start = min(x, na.rm = TRUE),
                       reset_monthly = FALSE) {
  if (reset_monthly) {
    if (!missing(start))
      warning("`start` is ignored when `reset_monthly` = TRUE")
    return(lubridate::floor_date(x, unit = paste(n, "days")))
  }
  start <- lubridate::as_date(start)
  days_since = as.numeric(lubridate::as_date(x) - start, units = "days")
  days_since_floored <- (days_since %/% n) * n
  start + days_since_floored
}

#' Number of days between two dates
#'
#' Returns number of days between two dates.
#'
#' @export
days_diff <- function(d1, d2) as.numeric(d2 - d1, unit = "days")

#' Cross-tabulate observations
#'
#' @description
#' Builds a contingency table similar to `base::table()`. Unlike
#' `base::table()`, `crosstab()` is pipe-friendly, and outputs an ordinary
#' tibble / data.frame -- e.g., it will retain its structure if exported to csv.
#'
#' Currently supports only two variables.
#'
#' @export
crosstab <- function(.data,
                     ...,
                     .drop = TRUE) {
  stopifnot(length(rlang::ensyms(...)) == 2)
  .data <- dplyr::count(.data, ..., .drop = .drop)
  names(.data)[[2]] <- "to_pivot"
  tidyr::pivot_wider(
    .data,
    names_from = to_pivot,
    values_from = n,
    values_fill = 0
  )
}

#' Count NA values by group
#'
#' Returns patterns of missingness across one or more variables, with number of
#' cases for each pattern.
#'
#' @export
count_na <- function(.data,
                     ...,
                     .label_missing = NA_character_,
                     .label_valid = "OK",
                     .add_percent = FALSE) {
  count_fx <- if (.add_percent) count_pct else dplyr::count
  .data %>%
    dplyr::select(...) %>%
    count_fx(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(.x), .label_missing, .label_valid)
      )
    )
}


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

#' Compute Cohen's w
#'
#' Cohen's w is an effect size measure for associations between nominal
#' variables, generally used in conjunction with chi-squared tests. `cohen_w()`
#' computes Cohen's w from results from a chi-squared test.
#'
#' @param chisq An `"htest"` object returned by `stats::chisq.test()`.
#'
#' @examples
#'
#' chisq_out <- chisq.test(ggplot2::diamonds$cut, ggplot2::diamonds$color)
#' cohen_w(chisq_out)
#'
#' @export
cohen_w <- function(chisq) {
  Sum <- sum(chisq$observed)
  Expected <- chisq$expected / Sum
  Observed <- chisq$observed / Sum
  sqrt(sum((Observed - Expected)^2 / Expected))
}

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


#' Add "Total" group to grouped dataframe
#'
#' @description
#' Groups a dataframe by columns specified in `...` using `dplyr::group_by()`,
#' and adds an additional group containing all observations. Useful for
#' including a "total" or "overall" row in summaries.
#'
#' If more than one column is passed to `...`, the "total" group will combine
#' all groups in the first column passed, unless a different column is specified
#' in `.totals_for`.
#'
#' Removing or changing the grouping structure after calling
#' `group_with_total()` but before aggregating may yield inaccurate results.
#'
#' @examples
#' ggplot2::mpg %>%
#'   group_with_total(class) %>%
#'   dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))
#'
#' ggplot2::mpg %>%
#'   group_with_total(year, drv, .label = "all years") %>%
#'   dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))
#'
#' ggplot2::mpg %>%
#'   group_with_total(year, drv, .totals_for = drv) %>%
#'   dplyr::summarize(n = dplyr::n(), cty = mean(cty), hwy = mean(hwy))
#'
#' @export
group_with_total <- function(.data,
                             ...,
                             .totals_for = NULL,
                             .label = "Total",
                             .add = FALSE,
                             .drop = dplyr::group_by_drop_default(.data),
                             .first_row = FALSE) {
  vars <- rlang::enquos(...)
  .totals_for <- rlang::enquo(.totals_for)
  .totals_for <- if (rlang::quo_is_null(.totals_for)) vars[[1]] else .totals_for
  .data <- dplyr::mutate(
    .data,
    !!.totals_for := forcats::fct_expand(factor(!!.totals_for), .label)
  )
  if (.first_row) {
    .data <- dplyr::mutate(
      .data,
      !!.totals_for := forcats::fct_relevel(!!.totals_for, .label)
    )
  }
  totals <- dplyr::mutate(.data, !!.totals_for := factor(
    .label,
    levels = levels(!!.totals_for)
  ))
  dplyr::group_by(
    dplyr::bind_rows(.data, totals),
    ...,
    .add = .add,
    .drop = .drop
  )
}

#' Maxima and minima with alternative missing value handling
#'
#' Returns the maximum or minimum of input values, similar to `base::min()` /
#' `max()`. Unlike these base functions, `safe_min()` and `safe_max()` return
#' `NA` when all values are `NA` and `na.rm = TRUE`. (`base::min` and `max()`
#' return `-Inf` or `Inf` in this situation). Also unlike base functions,
#' `na.rm` is `TRUE` by default (since this is the typical use case).
#'
safe_minmax <- function(..., na.rm, .fn) {
  all_dots <- c(...)
  if (all(is.na(all_dots))) {
    if (is.logical(all_dots)) NA_integer_
    else na_like(all_dots)
  } else {
    .fn(..., na.rm = na.rm)
  }
}

#' @rdname safe_minmax
#'
#' @export
safe_max <- function(..., na.rm = TRUE) {
  safe_minmax(..., na.rm = na.rm, .fn = max)
}

#' @rdname safe_minmax
#'
#' @export
safe_min <- function(..., na.rm = TRUE) {
  safe_minmax(..., na.rm = na.rm, .fn = min)
}

#' tidyselect-friendly parallel minima and maxima
#'
#' Wrappers around `base::pmin()` and `base::pmax()` that accept
#' <[`tidy-select`][dplyr_tidy_select]> expressions.
#'
#' @examples
#' # using `base::pmax()`
#' mtcars %>%
#'   dplyr::mutate(
#'     max_val = pmax(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
#'   )
#'
#' # using `pmax_across()`
#' mtcars %>%
#'   dplyr::mutate(max_val = pmax_across(mpg:carb))
#'
pminmax_across <- function(cols, na.rm, .fn) {
  col_names <- untidyselect(dplyr::cur_data(), !!cols, syms = TRUE)
  dplyr::cur_data() %>%
    dplyr::mutate(out = .fn(!!!col_names, na.rm = na.rm)) %>%
    dplyr::pull(out)
}

#' @rdname pminmax_across
#'
#' @export
pmax_across <- function(cols, na.rm = FALSE) {
  pminmax_across(rlang::enquo(cols), na.rm = na.rm, .fn = pmax)
}

#' @rdname pminmax_across
#'
#' @export
pmin_across <- function(cols, na.rm = FALSE) {
  pminmax_across(rlang::enquo(cols), na.rm = na.rm, .fn = pmin)
}


#' Scaling and centering of vectors
#'
#' A wrapper around `base::scale()` that returns a vector instead of a matrix.
#'
#' @examples
#' # using base::scale()
#' scale(0:4)
#'
#' # using scale_vec()
#' scale_vec(0:4)
#'
#' @export
scale_vec <- function(x, center = TRUE, scale = TRUE) {
  scale(x, center = center, scale = scale)[, 1]
}


#' Transpose a tibble
#'
#' Given a tibble or data.frame `x`, returns the transpose of `x`. Similar to
#' `base::t()`, but accepts and returns tibbles, and includes options for row
#' column name handling.
#'
#' @param x A dataframe or tibble.
#'
#' @param names_to Name for column in the transposed tibble containing column
#' names from `x`
#'
#' @param names_from Column in `x` used for column names of transposed tibble.
#' If not specified and `x` has rownames, these will be used; otherwise columns
#' will be named `...1`, `...2`, etc.
#'
#' @export
t_tibble <- function(x, names_to = "Variable", names_from = NULL) {
  if (!missing(names_from)) {
    names.t <- dplyr::pull(x, {{ names_from }})
    x <- dplyr::select(x, !{{ names_from }})
  }
  x.t <- t(x)
  if (!is.null(names_from)) colnames(x.t) <- names.t
  tibble::as_tibble(x.t, rownames = names_to, .name_repair = "unique")
}





#' Nicer default theme and palettes for `ggplot2`
#'
#' Changes default theme and color scales for `ggplot2`.
#'
#' Theme is based on `hrbrthemes::theme_ipsum_rc()`, unless otherwise specified
#' in `base_theme` argument. The theme is further modified as follows:
#' * Axis titles are centered
#' * Legend title is omitted
#' * Minor gridlines are omitted
#' * Facet labels are placed outside axes
#' * Various tweaks to text size and margins
#'
#' Default color and fill palettes are set based on scale type:
#' * For discrete scales, `RColorBrewer` palette "Set1," unless otherwise
#' specified in `brewer_pal_discrete` argument
#' * For continuous and binned scales, `RColorBrewer` palette "Blues"
#' * For ordinal scales, `viridisLite` palette "viridis"
#'
#' Default font family for `geom_text()` and `geom_label()` is set to match
#' `base_theme`.
#'
#' @export
set_ggplot_opts <- function(base_theme = hrbrthemes::theme_ipsum_rc,
                            brewer_pal_discrete = "Set1",
                            ...) {
  scale_color_brewer_d <- function(...) {
    ggplot2::scale_color_brewer(palette = brewer_pal_discrete, ...)
  }
  scale_fill_brewer_d <- function(...) {
    ggplot2::scale_fill_brewer(palette = brewer_pal_discrete, ...)
  }
  options(
    ggplot2.continuous.colour = ggplot2::scale_color_distiller,
    ggplot2.continuous.fill = ggplot2::scale_fill_distiller,
    ggplot2.binned.colour = ggplot2::scale_color_fermenter,
    ggplot2.binned.fill = ggplot2::scale_fill_fermenter,
    ggplot2.discrete.colour = scale_color_brewer_d,
    ggplot2.discrete.fill = scale_fill_brewer_d,
    ggplot2.ordinal.colour = ggplot2::scale_color_viridis_d,
    ggplot2.ordinal.fill = ggplot2::scale_fill_viridis_d
  )
  theme_family <- base_theme()$text$family
  ggplot2::update_geom_defaults("text", list(family = theme_family))
  ggplot2::update_geom_defaults("label", list(family = theme_family))
  ggplot2::theme_set(
    base_theme() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = 12,
          hjust = .5,
          margin = ggplot2::margin(t = 5)
        ),
        axis.title.y = ggplot2::element_text(
          size = 12,
          hjust = .5,
          margin = ggplot2::margin(r = 5)
        ),
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 11),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(15, 15, 15, 15),
        strip.placement = "outside",
        ...
      )
  )
}

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

#' Add empty rows at specified values in a column
#'
#' Adds empty row(s) based on specified value(s) in `col`. By default, will
#' insert one empty row after the last occurrence in `col` of each value passed
#' to `vals`.
#'
#' @param .data a data frame or data frame extension.
#' @param col column to search for values.
#' @param vals character vector of value(s) to search for in `col`.
#' @param where where to insert rows relative to the values in `vals`.
#' @param nomatch what to do if a value in `vals` doesn't appear in `col`.
#' @param nrows number of empty rows to insert at each location.
#' @param ... these dots are included to support error-checking and must be empty.
#'
#' @return An updated version of `.data` with new empty rows inserted.
#'
#' @details
#' In the pre-release version of this function, used in some lighthouse code, values to search for were passed to
#' `...` as unquoted symbols. Values must now be now instead be passed to `vals`
#' as a character vector. The arguments `.where` and `.nrows` have also been renamed to `where` and `nrows`. The function will attempt to detect and give an informative warning if it is called with "old" parameters (e.g., with a deprecated argument or with a list of symbols rather than a character vector).
#' Also see [`syms_to_chr()`], which is provided as a utility for adapting old code.
#'
#' @examples
#' set.seed(13)
#' ex_data <- tibble::tibble(
#'   category = sort(sample(LETTERS[1:3], 10, replace = TRUE)),
#'   var = round(runif(10), 2)
#' )
#'
#' add_rows_at_value(ex_data, category, c("A", "B"))
#'
#' add_rows_at_value(ex_data, category, c("A", "C"), where = "after_each")
#'
#' add_rows_at_value(
#'   ex_data,
#'   category,
#'   unique(ex_data$category),
#'   where = "before_first",
#'   nrows = 2
#' )
#'
#' @export
add_rows_at_value <- function(.data,
                              col,
                              vals,
                              where = c("after_last", "before_first", "after_each", "before_each"),
                              no_match = c("error", "warn", "ignore"),
                              nrows = 1,
                              ...) {
  find_loc <- function(val, col, where, no_match) {
    if (val %!in% col) {
      if (no_match == "error") {
        stop('Value "', val, '" not found in specified column.', call. = FALSE)
      } else if (no_match == "warn") {
        warning('Value "', val, '" not found in specified column.', call. = FALSE)
      }
      return(integer())
    }
    locs <- which(col == val)
    switch(where, after_last = max(locs), before_first = min(locs), locs)
  }
  #### check for pre-release usage patterns
  vals_chk <- rlang::enexpr(vals)
  where_chk <- rlang::enexpr(where)
  if (is.symbol(vals_chk) && (!exists(vals_chk, rlang::caller_env()) || is.symbol(where_chk))) {
    stop(
      "`add_rows_at_value()` no longer supports passing values as symbols. Instead, \n",
      "    pass values to `vals` as a character vector. `lighthouse::syms_to_chr()` may \n",
      "    be useful for updating code."
    )
  }
  dots <- list(...)
  if (length(dots) > 0) {
    dot_names <- names(dots)
    if (is.null(dot_names) || !all(dot_names %in% c(".where", ".nrows"))) {
      stop("`...` must be empty.")
    }
    if (".where" %in% dot_names) {
      warning("The `.where` argument is deprecated. Please use `where` instead.")
      where <- dots$.where
    }
    if (".nrows" %in% dot_names) {
      warning("The `.nrows` argument is deprecated. Please use `nrows` instead.")
      nrows <- dots$.nrows
    }
  }
  ####
  where <- match.arg(where)
  no_match <- match.arg(no_match)
  col <- dplyr::pull(.data, {{col}})
  locs <- lapply(vals, find_loc, col = col, where = where, no_match = no_match) |>
    unlist()
  if (where %in% c("after_last", "after_each")) {
    add_empty_rows(.data, .after = locs, .nrows = nrows)
  } else {
    add_empty_rows(.data, .before = locs, .nrows = nrows)
  }
}


#'  Reverse the order of rows in a table.
#'
#' @export
rev_rows <- function(.data) dplyr::arrange(.data, rev(dplyr::row_number()))


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


### is this made obsolete by updates to pivot_wider()? deprecate? will need to update other functions that call it.

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

#' Pad vectors to the same length
#'
#' This function takes one or more vectors and pads them with `NA` values to the length of the longest vector.
#'
#' @param ... One or more vectors.
#'
#' @return A list of vectors, all having the same length as the longest input vector.
#' @examples
#' pad_vectors(1:3, 1:5, 1:4)
#'
#' # supports list unpacking with `!!!` operator
#' pad_vectors(!!!list(1:3, 1:5, 1:4))
#'
#'
#' # one use case is assembling vectors of different lengths into a dataframe
#' # for example, to see unique column values at a glance:
#' unique_vals <- dplyr::starwars %>%
#'   dplyr::select(hair_color:eye_color) %>%
#'   lapply(unique)
#' pad_vectors(!!!unique_vals) %>%
#'   as.data.frame()
#'
#' @export
pad_vectors <- function(...) {
  dots <- rlang::list2(...)
  if (!all(sapply(dots, is.vector))) stop("All arguments must be vectors.")
  max_length <- max(sapply(dots, length))
  lapply(dots, `length<-`, max_length)
}

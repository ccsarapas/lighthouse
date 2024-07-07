#' Count observations with totals row
#'
#' This variant of [dplyr::count()] adds a row with column totals. Totals
#' are computed for the first column passed to `...` unless otherwise specified
#' in `totals_for`.
#'
#' @inheritParams dplyr::count
#' @param totals_for The variable to total for. If omitted, defaults to the first variable in `...`.
#' @param label The label for the totals row. Defaults to "Total".
#' @param first_row If `TRUE`, the totals row will be placed first in
#'   the output. If `FALSE` (default), it will be placed last.
#' @param .drop Handling of factor levels that don't appear in the data, passed
#'   on to [dplyr::group_by()]. If FALSE will include counts for empty groups
#'   (i.e. for levels of factors that don't exist in the data).
#'
#' @return A data frame with counts for each grouping level, along with a
#'   "totals" row with the column totals for the totaled variable.
#'
#' @family `count()` variants
#'
#' @examples
#' mtcars %>%
#'   count_with_total(cyl)
#'
#' mtcars %>%
#'   count_with_total(cyl, gear)
#'
#' mtcars %>%
#'   count_with_total(cyl, gear, totals_for = gear)
#'
#' @export
count_with_total <- function(.data,
                             ...,
                             totals_for = NULL,
                             label = "Total",
                             first_row = FALSE,
                             wt = NULL,
                             sort = FALSE,
                             name = NULL,
                             .drop = dplyr::group_by_drop_default()) {
  vars <- rlang::enquos(...)
  if (is.null(rlang::enexpr(totals_for))) {
    totals_for <- vars[[1]]
    rearrange <- FALSE
  } else {
    totals_for <- rlang::enquo(totals_for)
    rearrange <- TRUE
  }
  counts <- .data %>%
    dplyr::count(..., wt = {{wt}}, sort = sort, name = name, .drop = .drop) %>%
    dplyr::mutate(!!totals_for := as.character(!!totals_for))
  totals <- .data %>%
    dplyr::mutate(!!totals_for := label) %>%
    dplyr::count(..., wt = {{wt}}, sort = sort, name = name, .drop = .drop)
  if (first_row) {
    out <- dplyr::bind_rows(totals, counts)
  } else {
    out <- dplyr::bind_rows(counts, totals)
  }
  if (rearrange) {
    dplyr::arrange(
      out,
      !!!(vars[!purrr::map_lgl(vars, identical, rlang::enquo(totals_for))])
    )
  } else {
    out
  }
}

#' Count observations with percentage
#'
#' This variant of [dplyr::count()] includes a column showing percentage of total observations for each group.
#'
#' @inheritParams dplyr::count
#' @param ... Variables to group by. Will be passed to [dplyr::count()].
#' @param na.rm If `TRUE`, removes rows with `NA` values before calculations.
#' @param .by a selection of columns to group by for just this operation, functioning as an alternative to [dplyr::group_by()]. Percentages will be computed within each group rather than for the grand total. See examples.
#'
#' @return A data frame with columns for grouping variables, `n` (the count of observations in each group), and `pct` (the percentage of total observations in each group).
#'
#' @details
#' Percentages within subgroups can be obtained by grouping with `group_by`
#'
#' @family count variants
#'
#' @examples
#' library(dplyr)
#'
#' ## note effect of `na.rm` on percentages
#' dplyr::starwars %>%
#'   count_pct(gender)
#'
#' dplyr::starwars %>%
#'   count_pct(gender, na.rm = TRUE)
#'
#' ## note effect of grouping on percentages
#' # no grouping: % of grand total
#' ggplot2::mpg %>%
#'  count_pct(year, cyl)
#'
#' # grouping w `group_by()`: % of group, output is grouped
#' ggplot2::mpg %>%
#'   dplyr::group_by(year) %>%
#'   count_pct(cyl)
#'
#' # grouping w `.by`: % of group, output isn't grouped
#' ggplot2::mpg %>%
#'   count_pct(cyl, .by = year)
#'
#' @export
count_pct <- function(.data,
                      ...,
                      na.rm = FALSE,
                      .by = NULL,
                      wt = NULL,
                      sort = FALSE,
                      .drop = dplyr::group_by_drop_default()) {
  if (!is.null(rlang::enexpr(.by))) {
    if (dplyr::is_grouped_df(.data) || inherits(.data, "rowwise_df")) {
      stop(
        "Can't supply `.by` when `.data` is a grouped or rowwise data frame."
      )
    }
    .data |>
      dplyr::group_by(dplyr::pick({{.by}}), .drop = .drop) |>
      count_pct(
        ...,
        na.rm = na.rm,
        wt = {{wt}},
        sort = sort,
        .drop = .drop
      ) |>
      dplyr::ungroup()
  } else {
    cnt <- dplyr::count(
      .data,
      ...,
      wt = {{wt}},
      sort = sort,
      .drop = .drop
    )
    if (na.rm) cnt <- tidyr::drop_na(cnt)
    dplyr::mutate(cnt, pct = n / sum(n))
  }
}

#' Count observations for multiple variables
#'
#' This variant of [dplyr::count()] returns frequencies and (optionally) proportions for each column passed to `...`.
#'
#' @param .data A data frame.
#' @param ... Columns to count frequencies for. Can be named expressions.
#' @param .pct If `TRUE` (default), include percentages.
#' @param sort If `TRUE`, sort output by frequency.
#' @param name Name for the frequency column. Default is \"n\".
#' @param na.rm If `TRUE`, remove rows with `NA` values.
#' @param .by a selection of columns to group by for just this operation,
#'   functioning as an alternative to [dplyr::group_by()]. Percentages will be
#'   computed within each group rather than for the grand total. See examples.
#' @param .drop If `TRUE` (default), drop unused factor levels.
#'
#' @return A data frame with columns:
#'   - Any grouping variables from the input `data` or specified in `.by`.
#'   - `Variable`: The name of the column being counted.
#'   - `Value`: The unique values from the counted column.
#'   - `n`: The frequency of each unique value.
#'   - `pct`: (If `.pct = TRUE`) The percentage each count represents within its variable.
#'
#' @examples
#' iris %>%
#'   count_multiple(Species, Sepal.Length > 5)
#'
#' ## note effects of grouping
#' # no grouping
#' ggplot2::mpg %>%
#'   count_multiple(year, drv, cyl)
#'
#' # grouping w `group_by()`: counts and % nested within groups, output is grouped
#' ggplot2::mpg %>%
#'   dplyr::group_by(year) %>%
#'   count_multiple(drv, cyl)
#'
#' # grouping w `.by`: counts and % nested within groups, output isn't grouped
#' ggplot2::mpg %>%
#'   count_multiple(drv, cyl, .by = year)
#'
#' @export
count_multiple <- function(.data,
                           ...,
                           .pct = TRUE,
                           wt = NULL,
                           sort = FALSE,
                           name = NULL,
                           na.rm = FALSE,
                           .by = NULL,
                           .drop = TRUE) {
  name <- name %||% "n"
  args <- rlang::enexprs(...)
  if (!is.null(names(args))) {
    is_computed <- purrr::map_lgl(names(args), ~ stringr::str_length(.x) > 0)
    names(args)[!is_computed] <- args[!is_computed]
    .data <- dplyr::mutate(.data, !!!args)
    args <- rlang::syms(names(args))
  }
  if (!is.null(rlang::enexpr(.by))) {
    if (dplyr::is_grouped_df(.data) || inherits(.data, "rowwise_df")) {
      stop(
        "Can't supply `.by` when `.data` is a grouped or rowwise data frame."
      )
    }
    .data |>
      dplyr::group_by(dplyr::pick({{.by}}), .drop = .drop) |>
      count_multiple(
        ...,
        .pct = .pct,
        wt = {{wt}},
        sort = sort,
        name = name,
        na.rm = na.rm,
        .drop = .drop
      ) |>
      dplyr::ungroup()
  } else {
    grps <- dplyr::groups(.data)
    out <- purrr::map_dfr(
      args,
      ~ .data %>%
        dplyr::count(
          !!.x,
          sort = sort,
          wt = {{wt}},
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
}


#' Count duplicates across specified columns
#'
#' @description
#' This variant of [dplyr::count()] returns the number of duplicate observations across the specified columns.
#' Returns the number of unique duplicated values, as well as the total number
#' of duplicated observations.
#'
#' @param .data A data frame.
#' @param ... Columns to use for duplicate checks. If empty, all columns are used.
#' @param na.rm If `TRUE`, rows containing `NA` in the specified columns will be removed before counting duplicates.
#'
#' @return
#' A data frame with columns:
#' - `instances`: The number of times each unique value is duplicated
#' - `n_unique`: The number of unique values duplicated `instances` times
#' - `n_total`: The total number of observations duplicated `instances` times
#'
#' @examples
#' df <- tibble::tibble(
#'   x = c(1, 1, 2, 3, 3),
#'   y = c('a', 'a', 'b', 'c', 'c')
#' )
#'
#' count_duplicates(df)
#' count_duplicates(df, x)
#' count_duplicates(df, y)
#'
#' @export
count_duplicates <- function(.data, ..., na.rm = FALSE) {
  if(na.rm) .data <- tidyr::drop_na(.data, ...)
  .data %>%
    dplyr::count(..., name = "instances") %>%
    dplyr::group_by(instances) %>%
    dplyr::summarize(
      n_unique = dplyr::n(),
      n_total = dplyr::n() * instances[[1]]
    )
}

#' Count unique values in data frame columns
#'
#' This variant of [dplyr::count()] returns the number of unique values across a set of columns in a data frame.
#'
#' @param .data a data frame.
#' @param ... columns to count unique values across.
#' @param name name to give the unique count column.
#' @param na.rm exclude `NA`s from counts?
#'
#' @examples
#' mtcars %>%
#'   count_unique(cyl, gear)
#'
#' mtcars %>%
#'   count_unique(cyl, gear, carb, name = "unique_combos")
#'
#' @export
count_unique <- function(.data, ..., name = "n_unique", na.rm = FALSE) {
  .data %>%
    dplyr::summarize(
      !!name := dplyr::n_distinct(!!!rlang::enquos(...), na.rm = na.rm)
    )
}

##### add group / `.by` support #####
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


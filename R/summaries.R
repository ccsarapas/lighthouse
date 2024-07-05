
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
#' @param .cols_group_opts A list of additional arguments passed to
#' [`tidyr::pivot_wider()`] if `.cols_group_by` is specified.
#'
#' @param .var_col_name The name of the output column containing variable names
#' passed to `.vars`. This column will be dropped if `.var_col_name` is `NULL` and only one variable is passed to `.vars`.
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
#' # `.var_col_name = NULL` will drop the variable name column if only one
#' # variable is included in `.vars`
#' mtcars2 %>%
#'  summary_table(
#'    n = ~ sum(!is.na(.x)), m = mean, sd,
#'    .vars = mpg,
#'    .cols_group_by = cyl,
#'    .rows_group_by = Transmission,
#'    .var_col_name = NULL
#'  )
#'
#' # `.cols_group_opts` are passed as arguments to `pivot_wider()`.
#' # for instance to customize column names with a glue specification:
#' mtcars2 %>%
#'   summary_table(
#'     M = mean, SD = sd,
#'     .vars = c(mpg, hp, weight = wt),
#'     .cols_group_by = c(Transmission, cyl),
#'     .cols_group_opts = list(names_glue = "{cyl} cyl {Transmission}: {.value}")
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
  .fns <- lighthouse:::named_fn_list(...)
  if (is.null(.var_col_name)) {
    if (length(untidyselect(.data, {{ .vars }})) > 1) {
      stop("`.var_col_name` may not be `NULL` if more than one variable is specified in `.vars`.")
    }
    .var_col_name <- "..TMP_VAR_COL.."
  }
  .data <- dplyr::group_by(
    .data,
    dplyr::pick({{ .rows_group_by }}, {{ .cols_group_by }}),
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
  if (.var_col_name == "..TMP_VAR_COL..") {
    .data[[.var_col_name]] <- NULL
  }
  if (!missing(.cols_group_by)) {
    names_from_cols <- untidyselect(.data, {{ .cols_group_by }})
    rlang::exec(
      tidyr::pivot_wider,
      .data,
      !!!.cols_group_opts,
      names_from = tidyselect::all_of(names_from_cols),
      values_from = tidyselect::all_of(names(.fns)),
      names_vary = "slowest"
    )
  } else {
    .data
  }
}


#' Get information about data frame columns
#'
#' Returns a summary of each column's class, type, and missing data. If the
#' data frame was imported from an SPSS .sav file and the \"labelled\" package is
#' installed, SPSS variable labels will also be included.
#'
#' @param x A data frame.
#' @param zap_spss If `TRUE` (default) and the \"labelled\" package is available,
#'   convert SPSS-style labeled columns to standard R columns. Ignored if
#'   \"labelled\" is not installed.
#'
#' @return A tibble with a row for each column in `x`, containing:
#' - `column`: Column name
#' - `class`: Column class
#' - `type`: Column type
#' - `valid_n`: Number of non-missing values
#' - `valid_pct`: Percentage of non-missing values
#' - `label`: SPSS variable label (if applicable)
#'
#' @examples
#' cols_info(dplyr::starwars)
#'
#' @export
cols_info <- function(x, zap_spss = TRUE) {
  if (rlang::is_installed("labelled")) {
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

#' Summarize variables based on measurement level
#'
#' @description
#' Summarizes each variable passed to `...`. This is handled differently based
#' on each variable's level of measurement:
#' \itemize{
#'   \item For nominal variables, returns \emph{n} and proportion for each level
#'   \item For binary variables, returns \emph{n} and proportion `TRUE`
#'   \item For continuous variables, returns mean and standard deviation by
#'     default. Specify alternative summary statistics using `.cont_fx`.
#' }
#'
#' By default, `summary_report()` will guess the measurement level for each
#' variable. This can be overridden for all variables using the `.default`
#' argument, or for select variables using the `nom()`, `bin()`, or `cont()`
#' measurement wrappers. See details.
#'
#' @param .data a data frame or data frame extension.
#' @param ... <[`tidy-select`][dplyr_tidy_select]> one or more variable names.
#'   and/or tidyselect expressions. Elements may be wrapped in `nom()`, `bin()`,
#'   or `cont()` to force summarizing as binary, nominal, or continuous,
#'   respectively; see details.
#' @param .default how to determine measurement level for variables if not
#'   specified by a measurement wrapper. `"auto"` will guess measurement level
#'   for each variable, while `"nom"`, `"bin"`, and `"cont"` will treat all
#'   unwrapped variables as nominal, binary, or continuous, respectively.
#' @param .drop if `FALSE`, frequencies for nominal variables will include
#'   counts for empty groups (i.e. for levels of factors that don't exist in the
#'   data).
#' @param .cont_fx a list containing the two functions with which continuous
#'   variables will be summarized.
#' @param .missing_label label for missing values in nominal variables.
#' @param na.rm if `TRUE`, `NA` values in each variable will be dropped prior to
#'   computation.
#' @param na.rm.nom,na.rm.bin,na.rm.cont control `NA` handling specifically for
#'   nominal, binary, or continuous variables. Overrides `na.rm` for that
#'   variable type.
#'
#' @section Determining measurement level:
#' The measurement level for each variable is determined as follows:
#' \enumerate{
#'   \item Variables wrapped in `nom()`, `bin()`, or `cont()` will be treated
#'     as nominal, binary, or continuous, respectively.
#'   \item Variables without a measurement wrapper will be treated as the type
#'     specified in `.default`.
#'   \item If `.default` is `"auto"`, measurement level will be inferred:
#'   \itemize{
#'     \item Logical vectors will be treated as binary if there are no missing
#'       values or if `na.rm.bin = TRUE`.
#'     \item Character vectors, factors, and logical vectors with missing
#'       values will be treated as nominal.
#'     \item All other variables will be treated as continuous.
#'   }
#' }
#'
#' @section Support for binary variables:
#' To be treated as binary, both of these must be true:
#' \enumerate{
#'   \item The variable must be either a logical vector, or a binary numeric
#'     vector containing only 0s and 1s.
#'   \item The variable must not include any missing values, or `na.rm.bin`
#'     must be set to `TRUE`.
#' }
#' Future extensions may allow handling of other dichotomous variables (e.g.,
#' `"Pregnant"` vs. `"Not pregnant"`), but this is not currently supported.
#' Instead, consider converting these to a logical indicator, e.g., `Pregnant
#' = PregnancyStatus == "Pregnant"`.
#'
#' @examples
#' \dontrun{
#' # create a report using pre-processed SOR data
#' total_label <- "SOR-II Overall"
#' data_baseline %>%
#' group_with_total(ServiceType, .label = total_label) %>%
#'   summary_report(
#'     Age, Gender, Race,
#'     bin(DAUseAlcohol, DAUseIllegDrugs, DAUseBoth),
#'     DAUseAlcoholDays, DAUseIllegDrugsDays, DAUseBothDays,
#'     DAUseAlcoholDaysOrdinal, DAUseIllegDrugsDaysOrdinal, DAUseBothDaysOrdinal,
#'     na.rm = TRUE,
#'     .drop = FALSE
#'   ) %>%
#'   pivot_wider(
#'     names_from = ServiceType,
#'     names_vary = "slowest",
#'     values_from = V1:V2
#'   ) %>%
#'   relocate(contains(total_label), .after = Value) %>%
#'   add_rows_at_value(Variable, Race, DAUseBoth, DAUseBothDays) %>%
#'   print_all()
#' }
#'
#' @export
summary_report <- function(.data,
                           ...,
                           .default = c("auto", "nom", "bin", "cont"),
                           .drop = TRUE,
                           .cont_fx = list(mean, sd),
                           .missing_label = NA,
                           na.rm = FALSE,
                           na.rm.nom = na.rm,
                           na.rm.bin = na.rm,
                           na.rm.cont = na.rm) {
  process_args <- function(..., .env) {
    process_arg <- function(arg, env) {
      autotype <- function(var) {
        if (
          is.factor(.data[[var]]) ||
          typeof(.data[[var]]) == "character" ||
          (typeof(.data[[var]]) == "logical" &&
           !na.rm.bin &&
           any(is.na(.data[[var]])))
        ) "nom"
        else if (typeof(.data[[var]]) == "logical") "bin"
        else "cont"
      }
      check_cont <- function(var) {
        v <- .data[[var]]
        if (is.factor(v)) stop(var, " set as continuous but is a factor.")
        if (is.character(v)) stop(var, " set as continuous but is character.")
      }
      check_bin <- function(var) {
        if (dplyr::n_distinct(.data[[var]], na.rm = na.rm.bin) > 2) {
          stop(
            var,
            " set as binary but has >2 unique values. This sometimes",
            " means there are missing values and `na.rm = FALSE`."
          )
        }
      }
      if (rlang::is_call(arg)
          && rlang::call_name(arg) %in% c("nom", "bin", "cont")) {
        vars <- rlang::call_args(arg)
        types <- rlang::call_name(arg)
      } else {
        vars <- arg
        types <- .default
      }
      ######
      ## 2024-05-16 - changed to handle case where `...` refers to variable
      ##     defined in calling environment (e.g. `all_of(varlist)`), including
      ##     below and adding and specifying `.env` / `env` arguments
      ## removed:
      # vars <- untidyselect(.data, c(!!!vars))
      ## added:
      vars <- names(tidyselect::eval_select(rlang::expr(c(!!!vars)), .data, env = env))
      ######
      types <- switch(
        types,
        auto = purrr::map_chr(vars, autotype),
        cont = {
          purrr::walk(vars, check_cont)
          rlang::rep_along(vars, types)
        },
        bin = {
          purrr::walk(vars, check_bin)
          rlang::rep_along(vars, types)
        },
        nom = rlang::rep_along(vars, types)
      )
      list(types = types, vars = purrr::map(vars, as.symbol), vars_chr = vars)
    }
    rlang::enexprs(...) %>%
      purrr::map(process_arg, env = .env) %>%
      purrr::transpose() %>%
      purrr::map(unlist)
  }
  Mode <- function(x, ...) {
    if (all(is.na(x))) {
      na_like(x)
    } else {
      names(sort(table(x[!is.na(x)]), decreasing = TRUE))[[1]]
    }
  }
  summarize_bin <- function(.data, bin_vars) {
    bin_data <- dplyr::select(dplyr::ungroup(.data), !!!bin_vars)
    bin_logical <- untidyselect(
      bin_data,
      where(
        ~ is.logical(.x) |
          (is.numeric(.x) & (all(.x %in% c(0, 1) | is.na(.x))))
      ),
      syms = TRUE
    )
    bin_nominal <- untidyselect(bin_data, !c(!!!bin_logical), syms = TRUE)
    bin_out <- list()
    if (length(bin_logical) > 0) {
      bin_out[["logical"]] <- summary_table(
        .data,
        Value = ~ ifelse(is.logical(.x), "TRUE", "1"),
        V1 = ~ sum(.x, na.rm = na.rm.bin),
        V2 = ~ mean(.x, na.rm = na.rm.bin),
        .vars = c(!!!bin_logical)
      )
    }
    if (length(bin_nominal) > 0) {
      stop(
        "`bin()` supports only logical or binary numeric variables at this time.\n",
        "These variables are not supported:\n",
        paste(names(bin_nominal), collapse = " ")
      )
    }
    dplyr::bind_rows(bin_out)
  }
  .default <- match.arg(.default)
  caller <- rlang::caller_env()
  all_vars <- process_args(..., .env = caller)
  cont_fx_chr <- paste(
    rlang::call_args(rlang::enexpr(.cont_fx)),
    collapse = ", "
  )
  cont_vars <- all_vars$vars[all_vars$types == "cont"]
  if (length(cont_vars) > 0) {
    cont_out <- .data %>%
      summary_table(
        V1 = .cont_fx[[1]], V2 = .cont_fx[[2]], na.rm = na.rm.cont,
        .vars = c(!!!cont_vars)
      ) %>%
      dplyr::mutate(Value = cont_fx_chr, .after = Variable)
  } else {
    cont_out <- tibble::tibble()
  }
  bin_vars <- unlist(all_vars$vars[all_vars$types == "bin"])
  bin_out <- if (length(bin_vars) > 0) summarize_bin(.data, bin_vars) else tibble::tibble()
  nom_vars <- unlist(all_vars$vars[all_vars$types == "nom"])
  if (length(nom_vars) > 0) {
    nom_out <- .data %>%
      count_multiple(!!!nom_vars, na.rm = na.rm.nom, .drop = .drop) %>%
      dplyr::rename(V1 = n, V2 = pct)
  } else {
    nom_out <- tibble::tibble()
  }
  out <- dplyr::bind_rows(cont_out, bin_out, nom_out) %>%
    dplyr::arrange(factor(Variable, levels = all_vars$vars_chr))
  if (!missing(.missing_label)) {
    out <- dplyr::mutate(out, Value = tidyr::replace_na(Value, .missing_label))
  }
  out
}

meas_wrap_error <- function(...) {
  stop("`nom()`, `bin()`, and `cont()` can only be used inside `summary_report()`.")
}

#' @rdname summary_report
#' @export
nom <- meas_wrap_error

#' @rdname summary_report
#' @export
bin <- meas_wrap_error

#' @rdname summary_report
#' @export
cont <- meas_wrap_error


#' Compare two data frames and show differences
#'
#' Given two data frames with the same dimensions and column order, returns a data frame including only rows and columns with differences.
#'
#' @param x,y a pair of data frames
#' @param suffix suffixes to indicate source data frame in output.
#' @param keep <[`tidy-select`][dplyr_tidy_select> Columns to include in the output even if they have no differences.
#'
#' @return A data frame with rows and columns that have differing values between
#'   `x` and `y`. Differing columns are included twice, with suffixes appended.
#'   Columns specified in `keep` are always included.
#'
#' @examples
#' x <- data.frame(id = 1:3, A = c(7, 8, 9), B = c(10, 20, 30), C = c("x", "y", "z"))
#' y <- data.frame(id = 1:3, A = c(7, 8, 99), B = c(10, 20, 30), C = c("X", "y", "Z"))
#'
#' df_compare(x, y)
#' df_compare(x, y, keep = id)
#'
#' @export
df_compare <- function(x, y, suffix = c(".x", ".y"), keep = NULL) {
  eq_na <- function(x, y) {
    (!is.na(x) & !is.na(y) & x == y) | (is.na(x) & is.na(y))
  }
  stopifnot(all(dim(x) == dim(y)))
  nms <- names(x)
  stopifnot(all(nms == names(y)))
  df_diff <- !eq_na(x, y)
  rows_diff <- apply(df_diff, 1, any)
  cols_diff <- nms[apply(df_diff, 2, any)]
  keep <- rlang::enexpr(keep)
  if (!is.null(keep)) {
    keep <- untidyselect(x, !!keep)
    keep <- setdiff(keep, cols_diff)
  }
  x_diff <- x[rows_diff, c(keep, cols_diff)]
  y_diff <- y[rows_diff, cols_diff]
  names(x_diff) <- c(keep, stringr::str_c(cols_diff, suffix[[1]]))
  names(y_diff) <- stringr::str_c(cols_diff, suffix[[2]])
  out <- dplyr::bind_cols(x_diff, y_diff)
  out[c(
    keep,
    stringr::str_c(rep(cols_diff, each = 2), rep(suffix, length(cols_diff)))
  )]
}

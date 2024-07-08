
#' Utilities for service cascades
#'
#' Functions for working with service cascades.
#'
#' @name cascade
#'
#' @export
cascade_fill_bwd <- function(data, vars) {
  vars <- untidyselect(data, !!rlang::enexpr(vars))
  if (length(vars) < 2) stop("At least two variables required.")
  if (!(all(sapply(data[vars], is.logical)))) stop("All variabes must be logical.")
  for (i in seq(length(vars), 2)) {
    data[[vars[[i - 1]]]] <- data[[vars[[i]]]] | data[[vars[[i - 1]]]]
  }
  data
}
#' @rdname cascade
#' @export
cascade_fill_fwd <- function(data,
                             vars,
                             fill = c("NA", "FALSE")
                             ## maybe implement, to distinguish missings from "true" FALSEs
                             # initial_NAs_as = c("FALSE", "missing")
) {
  vars <- untidyselect(data, !!rlang::enexpr(vars))
  if (length(vars) < 2) stop("At least two variables required.")
  if (!(all(sapply(data[vars], is.logical)))) stop("All variabes must be logical.")
  fill <- match.arg(fill)
  # initial_NAs_as <- match.arg(initial_NAs_as)
  # if (initial_NAs_as == "missing") {
  ## handle `initial_NAs_as = "missing"`
  # } else {
  data[vars][is.na(data[vars])] <- FALSE
  if (fill == "NA") {
    for (i in seq(2, length(vars))) {
      data[[vars[[i]]]] <- dplyr::if_else(
        is_FALSE_or_NA(data[[vars[[i - 1]]]]),
        NA,
        data[[vars[[i]]]]
      )
    }
  }
  # }
  data
}
#' @rdname cascade
#' @export
cascade_summarize <- function(data, vars) {
  out <- summary_table(
    data,
    n = sum, n_denom = n_valid, pct_step = mean,
    pct_tot = \(x, ...) sum(x, na.rm = TRUE) / dplyr::n(),
    na.rm = TRUE,
    .vars = {{ vars }},
    .var_col_name = "Step"
  )
  out$Step <- forcats::fct_inorder(out$Step)
  out
}

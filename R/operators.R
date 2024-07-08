#' Unpack and assign
#'
#' Assigns a vector of values to an equal-length vector of names.
#'
#' The `%<-%` operator and its left-to-right variant (`%->%`) allow parallel
#' assignment, similar to (e.g.) Python:
#' ```
#' # python
#' var1, var2, var3 = [2, 4, 6]
#'
#' # R
#' c(var1, var2, var3) %<-% c(2, 4, 6)
#' ```
#' See the `zeallot` package for a
#' similar operator with more advanced functionality.
#'
#' @examples
#' c(cyl4, cyl6, cyl8) %<-% split(mtcars, mtcars$cyl)
#' cyl4
#'
#' split(mtcars, mtcars$gear) %->% c(G3, G4, G5)
#' G5
#'
#' c(model_hp, model_size, model_trans) %<-% purrr::map(
#'   c("hp", "disp + wt", "gear * am"),
#'   ~ lm(paste("mpg ~", .x), data = mtcars)
#' )
#' summary(model_size)
#' summary(model_trans)
#'
#' @name unpack-assign
#'
#' @export
`%<-%` <- function(lhs, rhs, envr = rlang::caller_env()) {
  char_if_valid <- function(x) {
    if (is.symbol(x) || rlang::is_scalar_character(x)) {
      as.character(x)
    } else {
      stop("Invalid component in left-hand side")
    }
  }
  lhs <- rlang::enexpr(lhs)
  if (is.call(lhs)) {
    fn <- lhs[[1]]
    if (fn == rlang::expr(c)) {
      lhs <- purrr::map_chr(lhs[-1], char_if_valid)
      if (length(lhs) %in% c(1L, length(rhs))) {
        invisible(purrr::map2(lhs, rhs, ~ assign(.x, .y, pos = envr)))
      } else {
        stop("Left-hand side must be same length as right-hand side, or length 1")
      }
    } else if (fn == rlang::expr(`%<-%`)) {
      rightmost <- `%<-%`(!!lhs[[3]], rhs, envr = envr)
      `%<-%`(!!lhs[[2]], rightmost, envr = envr)
    }
  } else if (is.symbol(lhs) || rlang::is_scalar_character(lhs)) {
    lhs <- as.character(lhs)
    assign(lhs, rhs, pos = envr)
  } else {
    stop("Invalid left-hand side")
  }
}

#' @rdname unpack-assign
#'
#' @export
`%->%` <- function(lhs, rhs) {
  char_if_valid <- function(x) {
    if (is.symbol(x) || rlang::is_scalar_character(x)) {
      as.character(x)
    } else {
      stop("Invalid component in right-hand side")
    }
  }
  envr <- rlang::caller_env()
  rhs <- rlang::enexpr(rhs)
  if (is.call(rhs) && rhs[[1]] == rlang::expr(c)) {
    rhs <- purrr::map_chr(rhs[-1], char_if_valid)
    if (length(rhs) %in% c(1L, length(lhs))) {
      invisible(purrr::map2(rhs, lhs, ~ assign(.x, .y, pos = envr)))
    } else {
      stop("Right-hand side must be same length as left-hand side, or length 1")
    }
  } else if (is.symbol(rhs) || rlang::is_scalar_character(rhs)) {
    rhs <- as.character(rhs)
    assign(rhs, lhs, pos = envr)
  } else {
    stop("Invalid right-hand side")
  }
}

#' Match values not in vector
#'
#' Infix operator returning `TRUE` for elements of left operand (`lhs`) not
#' found in right operand (`rhs`). Equivalent to `!(lhs %in% rhs)`.
#'
#' @examples
#'
#' "April" %!in% month.name
#' "Junvember" %!in% month.name
#'
#' some_letters <- sample(letters, 10)
#' letters[letters %in% some_letters]
#' letters[letters %!in% some_letters]
#'
#' @name not-in
#'
#' @export
`%!in%` <- function(lhs, rhs) !(lhs %in% rhs)


#' Test whether multiple values are in a vector
#'
#' These infix operators test whether any or all left-hand side elements occur
#' in the right-hand side.
#'
#' `%all_in%` returns `TRUE` if all elements of left operand (`lhs`) are found
#' in right operand (`rhs`). Equivalent to `all(lhs %in% rhs)`.
#'
#' `%any_in%` returns `TRUE` if any elements of left operand (`lhs`) are found
#' in right operand (`rhs`). Equivalent to `any(lhs %in% rhs)`.
#'
#' @examples
#'
#' maybe_states <- c("Idaho", "Illinois", "North Tuba", "Maine")
#' maybe_states %any_in% state.name  # TRUE
#' maybe_states %all_in% state.name  # FALSE
#' rm(maybe_states)
#'
#' @name any-all-in
#'
#' @export
`%all_in%` <- function(lhs, rhs) all(lhs %in% rhs)

#' @rdname any-all-in
#'
#' @export
`%any_in%` <- function(lhs, rhs) any(lhs %in% rhs)


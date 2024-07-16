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

#' Parallel / row-wise sums and means
#'
#' @description
#' Given one or more vectors, `psum()` and `pmean()` return row-wise sums or means. They are analogous to [`base::pmin()`] and [`pmax()`]. Like `pmin()` / `pmax()`, these functions:
#' - accept multiple vectors via `...` (as opposed to `rowSums()` and `rowMeans()`, which accept a single array). This allows them to be used in data-masking contexts (e.g., inside `dplyr::mutate()`).
#' - return `NA` when all values are `NA` and `na.rm = TRUE`. (as opposed to `rowSums()`, which returns `0` in this situation).
#'
#'
#' @param ... one or more numeric or logical vectors of equal length or length 1.
#' @param na.rm logical. Should missing values (including `NaN`) be removed?
#'
#' @return
#' A numeric vector.
#'
#' @details
#' Both `psum()` and `psum_across()` can be used in data-masking contexts, but
#' have different capabilities and limitations. See the "Details" section of
#' [`?psum_across`][`psum_across()`].
#'
#' Note that, unlike `pmin()`, `pmax()`, `rowSums()`, and `rowMeans()`, these
#' functions do not currently support objects with more than one dimension
#' (e.g., matrices, arrays, or data frames).
#'
#' @seealso
#' - [`psum_across()`] for variants that support tidyselect expressions. See the
#' "Details" section for relative strengths of `psum()` / `pmean()` vs.
#' `psum_across()` / `pmean_across()`.
#' - [`base::pmin()`] for analogous `pmin()` and `pmax()` functions
#' - [`sum_if_any()`] for non-parallel sums with similar `NA` handling
#'
#' @examples
#' psum(1:5, 6:10)
#' pmean(1:5, 6:10)
#'
#' dat <- tibble::tribble(
#'   ~product,    ~price1, ~price2, ~price3,
#'   "Product 1", 20,      25,      22,
#'   "Product 2", NA,      30,      29,
#'   "Product 3", 15,      NA,      NA,
#'   "Product 4", NA,      NA,      NA
#' )
#'
#' # contrast w `rowSums()` / `rowMeans()`:
#' # no need for `pick()` and different `NA` behavior
#' dat %>%
#'   dplyr::mutate(
#'     rowSums = rowSums(pick(price1, price2, price3), na.rm = TRUE),
#'     psum = psum(price1, price2, price3, na.rm = TRUE),
#'     rowMeans = rowMeans(pick(price1, price2, price3), na.rm = TRUE),
#'     pmean = pmean(price1, price2, price3, na.rm = TRUE)
#'   )
#'
#' @export
psum <- function(..., na.rm = FALSE) {
  pfx(..., na.rm = na.rm, .caller = "`psum()`", .fx = rowSums)
}
#'
#' @rdname psum
#' @export
pmean <- function(..., na.rm = FALSE) {
  pfx(..., na.rm = na.rm, .caller = "`pmean()`", .fx = rowMeans)
}

pfx <- function(..., na.rm, .caller, .fx) {
  if (any(sapply(list(...), \(x) length(dim(x))) > 1)) {
    stop(.caller, " does not support objects with more than one dimension.")
  }
  check_dots <- vctrs::vec_recycle_common(...)
  cols <- cbind(...)
  out <- .fx(cols, na.rm = na.rm)
  if (na.rm) out <- dplyr::if_else(rowSums(is_valid(cols)) == 0, NA, out)
  out
}

#' Sums, maxima and minima with alternative missing value handling
#'
#' Returns the sum, maximum, or minimum of input values, similar to
#' `base::sum()`, `min()`, and `max()`. Unlike these base functions, these
#' variants return `NA` when all values are `NA` and `na.rm = TRUE`.
#' (`base::sum()`, `min()`, and `max()` return `0`, `-Inf`, and `Inf`,
#' respectively, in this situation). Also unlike base functions, `na.rm` is
#' `TRUE` by default (since this is the typical use case).
#'
#' @param ... numeric, logical, or (for `max_if_any()` and `min_if_any()`) character vectors.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @examples
#' some_na <- c(1, 2, NA)
#' all_na <- c(NA, NA, NA)
#'
#' # unlike base functions, `na.rm = TRUE` by default
#' max(some_na)
#' max_if_any(some_na)
#'
#' # unlike base functions, returns `NA` when `na.rm = TRUE` and all inputs are `NA`
#' sum(all_na, na.rm = TRUE)
#' sum_if_any(all_na)
#'
#' @name aggregate_if_any
#' @export
sum_if_any <- function(..., na.rm = TRUE) {
  aggregate_if_any(..., na.rm = na.rm, .fn = sum)
}
#' @rdname aggregate_if_any
#' @export
max_if_any <- function(..., na.rm = TRUE) {
  aggregate_if_any(..., na.rm = na.rm, .fn = max)
}
#' @rdname aggregate_if_any
#' @export
min_if_any <- function(..., na.rm = TRUE) {
  aggregate_if_any(..., na.rm = na.rm, .fn = min)
}

aggregate_if_any <- function(..., na.rm, .fn) {
  if (all(is.na(c(...)))) .fn(..., na.rm = FALSE) else .fn(..., na.rm = na.rm)
}


#' tidyselect-friendly row-wise aggregation
#'
#' Wrappers around `base::pmin()`, `base::pmax()`, `lighthouse::psum()`, and
#' `lighthouse::pmean()` that accept
#' [`tidyselect expressions`][dplyr_tidy_select].
#'
#' @param ... <[`tidy-select`][dplyr_tidy_select]> one or more tidyselect
#'   expressions that capture numeric and/or logical columns.
#' @param na.rm Should missing values (including `NaN`) be removed?
#'
#' @details
#' Lighthouse includes two sets of functions for computing "parallel" or
#' row-wise aggregates:
#' - [`psum()`] and [`pmean()`] (which complement `base::pmin()` and `pmax()`)
#' - `pmin_across()`, `pmax_across()`, `psum_across()`, and `pmean_across()`
#'
#' Both sets of functions differ from `base::rowSums()` and `rowMeans()` in that
#' they:
#' - work in data-masking contexts (e.g., inside `dplyr::mutate()`) without
#' needing helpers like `dplyr::pick()` or `dplyr::across()`.
#' - accept multiple inputs via `...`.
#' - return `NA` when `na.rm = TRUE` and all values in a row are `NA`. This
#' mirrors behavior of `base::pmin()` and `pmax()`, but differs from
#' `rowSums()`, which returns `0` in this situation.
#'
#' `psum_across()` and friends support tidyselect expressions; e.g.,
#' \preformatted{
#' dat %>%
#'   mutate(
#'     IDScrTotal = psum_across(IDScr1:IDScr6),
#'     SDScrTotal = psum_across(starts_with("SDScr"))
#'   )
#' }
#'
#' ...but must be used inside a data-masking verb like `dplyr::mutate()`,
#' `group_by()`, or `filter()`, and do not support implicit computations.
#'
#' Conversely, `psum()` and friends do not support tidyselect expressions, but
#' can be used both inside or outside a data-masking context:
#' \preformatted{
#' # data-masking
#' dat %>%
#'   mutate(
#'     NumColors = psum(Red, Blue, Green),
#'   )
#'
#' #non-data masking
#' psum(1:10, 6:15, 11:20)
#' }
#'
#' and support "on the fly" or "implicit" computations:
#' \preformatted{
#' dat %>%
#'   mutate(
#'     CriteriaMet = psum(a1 > a2, pmax(b1, b2, b3) > 5, c1 < 10),
#'     TotalProp = psum(num1 / denom1, num2 / denom2, num3 / denom3)
#'   )
#' }
#'
#' @seealso
#' [`psum()`] and [`pmax()`] for non-tidyselect parallel aggregates.
#'
#'
#' @examples
#' dat <- tibble::tribble(
#'   ~product,    ~price1, ~price2, ~price3,
#'   "Product 1", 20,      25,      22,
#'   "Product 2", NA,      30,      29,
#'   "Product 3", 15,      NA,      NA,
#'   "Product 4", NA,      NA,      NA
#' )
#'
#' price_cols <- c("price1", "price2", "price3")
#'
#' dat %>%
#'   dplyr::mutate(
#'     min = pmin_across(price1, price2, price3, na.rm = TRUE),
#'     max = pmax_across(price1:price3, na.rm = TRUE),
#'     sum = psum_across(starts_with("price"), na.rm = TRUE),
#'     mean = pmean_across(all_of(price_cols), na.rm = TRUE)
#'   )
#'
#' @export
psum_across <- function(..., na.rm = FALSE) {
  pfx_across(..., na.rm = na.rm, .fx= psum)
}
#' @name psum_across
#' @export
pmean_across <- function(..., na.rm = FALSE) {
  pfx_across(..., na.rm = na.rm, .fx= pmean)
}
#' @name psum_across
#' @export
pmin_across <- function(..., na.rm = FALSE) {
  pfx_across(..., na.rm = na.rm, .fx= pmin)
}
#' @name psum_across
#' @export
pmax_across <- function(..., na.rm = FALSE) {
  pfx_across(..., na.rm = na.rm, .fx= pmax)
}

pfx_across <- function(..., na.rm, .fx) {
  do.call(.fx, c(as.list(dplyr::pick(...)), list(na.rm = na.rm)))
}


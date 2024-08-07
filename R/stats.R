#' Convert between probabilities and odds ratios
#'
#' These functions convert between probabilities and odds ratios. `p_to_OR` calculates the odds ratio given two probabilities, while `OR_to_p2` and `OR_to_p1` calculate the second and first probability, respectively, given the other probability and the odds ratio.
#'
#' @param p1 The first probability.
#' @param p2 The second probability.
#' @param OR The odds ratio.
#'
#' @return
#' - `p_to_OR`: The odds ratio corresponding to the given probabilities.
#' - `OR_to_p2`: The second probability corresponding to the given first probability and odds ratio.
#' - `OR_to_p1`: The first probability corresponding to the given second probability and odds ratio.
#'
#' @examples
#' p_to_OR(0.4, 0.6)
#' OR_to_p2(0.4, 2.25)
#' OR_to_p1(0.6, 2.25)
#'
#' @export
p_to_OR <- function(p1, p2) (p2 / (1 - p2)) / (p1 / (1 - p1))
#' @rdname p_to_OR
#' @export
OR_to_p2 <- function(p1, OR) {
  (OR * (p1 / (1 - p1))) / (1 + (OR * (p1 / (1 - p1))))
}
#' @rdname p_to_OR
#' @export
OR_to_p1 <- function(p2, OR) {
  ((p2 / (1 - p2)) * (1 / OR)) / (1 + ((p2 / (1 - p2)) * (1 / OR)))
}


#' Compute the standard error of the mean
#'
#' Computes the standard error of the mean for a numeric vector.
#'
#' @param x a numeric vector or a non-factor object coercible to numeric by `as.double(x)`.
#' @param na.rm	logical. Should missing values be removed?
#'
#' @export
se_mean <- function(x, na.rm = FALSE) {
  stats::sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
}


#' Compute the standard error of a proportion
#'
#' Computes the standard error of a proportion for a logical or numeric vector.
#'
#' @param x a logical or numeric vector. If numeric, it must include only 0s, 1s, and/or `NA`s.
#' @param na.rm	logical. Should missing values be removed?
#' @param min_var numeric. Minimum variance (n * p * (1 - p)) for valid normal approximation of the binomial. See Details.
#' @param low_var_action character. Action to take when variance is below `min_var`.
#'
#' @details
#' Standard error of a proportion is calculated using the formula:
#'
#' \deqn{SE = \sqrt{\frac{p(1 - p)}{n}}}
#'
#' This formula assumes that the binomial sampling distribution underlying the
#' observed proportion can be approximated by a normal distribution. This
#' assumption is valid when the proportion variance (n*p*(1 - p)) is
#' sufficiently large, but may not hold when variance is lower. `se_prop()`
#' therefore issues a warning when the variance is less than 5; this behavior
#' can be overridden using the `min_var` and `low_var_action` arguments.
#'
#' @export
se_prop <- function(x,
                    na.rm = FALSE,
                    min_var = 5,
                    low_var_action = c("warn", "ignore", "error")) {
  low_var_action <- match.arg(low_var_action)
  if (!(is.logical(x) || (is.numeric(x) && x %all_in% c(0, 1, NA)))) {
    stop("`x` must be a logical vector or a numeric vector containing only 0s and 1s.")
  }
  p <- mean(x, na.rm = na.rm)
  if (is.na(p)) return(p)
  q <- 1 - p
  n <- n_valid(x)
  npq <- n * p * q
  if (low_var_action != "ignore" && npq < min_var) {
    if (low_var_action == "error") {
      stop(
        "The variance (n*p*q = ", round(npq, 3), ") is < ", round(min_var, 3), ". See `?se_prop`."
      )
    }
    warning(
      "Result may be unreliable due to low variance (n*p*q = ", round(npq, 3), "). See `?se_prop`."
    )
  }
  sqrt((p * q)/n)
}


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
  rlang::check_installed(c("broom", "psych"))
  .data %>%
    dplyr::select({{x}}, {{y}}) %>%
    as.data.frame() %>%
    psych::cohen.kappa() %>%
    broom::tidy()
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

#' Test whether a confidence interval excludes a given value
#'
#' Tests whether a confidence interval excldes a specified reference value. This
#' will generally be the null value for the relevant test, such that excluding
#' this value indicates the test is statistically significant.
#'
#' @param ll a numeric vector containing confidence interval lower limits.
#' @param ul a numeric vector containing corresponding upper limits.
#' @param reference the value to check against. This will generally be the null
#'   value for the relevant test (e.g., 1 for odds ratios, 0 for beta
#'   coefficients).
#' @param return if `"logical"`, will return a logical vector indicating whether
#'   each confidence interval excludes `reference`. If `asterisks`, will return a
#'   character vector, using the characters passed to `marks`.
#' @param marks a length-2 vector specifying strings to mark significant and
#'   non-significant results when `return = "asterisks"`.
#'
#' @return
#' - If `return = \"logical\"` (default), a logical vector.
#' - If `return = \"asterisks\"`, a character vector.
#'
#' @examples
#' beta_CIs <- glm(
#'     Survived ~ Sex * Age,
#'     family = binomial,
#'     weights = Freq,
#'     data = as.data.frame(Titanic)
#'   ) %>%
#'   confint()
#' OR_CIs <- tibble::as_tibble(exp(beta_CIs), rownames = "term")
#' beta_CIs <- tibble::as_tibble(beta_CIs, rownames = "term")
#'
#' beta_CIs %>%
#'   dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`))
#'
#' beta_CIs %>%
#'   dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`, return = "asterisks"))
#'
#' beta_CIs %>%
#'   dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`, return = "asterisks", marks = c("*", "ns")))
#'
#' OR_CIs %>%
#'   dplyr::mutate(sig = ci_sig(`2.5 %`, `97.5 %`, reference = 1, return = "asterisks"))
#'
#' @export
ci_sig <- function(ll,
                   ul,
                   reference = 1,
                   return = c("logical", "asterisks"),
                   marks = c("*", NA_character_)) {
  return <- match.arg(return)
  out <- !(ll <= reference & ul >= reference)
  if (return == "logical") return(out)
  if (length(marks) != 2) stop("`marks` must be a vector of length 2.")
  ifelse(out, marks[[1]], marks[[2]])
}

#' Return asterisks corresponding to p-values
#'
#' Returns asterisks indicating significance levels for a vector of p values.
#'
#' @param p a numeric vector of p-values.
#' @param trends logical. Should trends (e.g., .05 < p < .10) be marked?
#' @param levels a numeric vector demarcating ranges of p values that should receive unique significance marks.
#' @param marks a named character vector specifying marks for significance, trend, and non-significance.
#' @param include_key logical. should a key for significance marks be included as an attribute?
#'
#' @return A character vector of asterisks corresponding to the p-values. If `include_key = TRUE`, the vector will have a 'key' attribute indicating the significance levels.
#'
#' @examples
#' p <- c(0.5, 0.09, 0.03, 0.008, 0.0003)
#'
#' tibble::tibble(p, sig = asterisks(p))
#'
#' tibble::tibble(p, sig = asterisks(p, trends = FALSE))
#'
#' tibble::tibble(p, sig = asterisks(p, trends = FALSE, marks = c(ns = "ns")))
#'
#' asterisks(p, include_key = TRUE)
#'
#' @export
asterisks <- function(p,
                      trends = TRUE,
                      levels = c(.1, .05, .01, .001),
                      marks = c(sig = "*", trend = "+", ns = NA_character_),
                      include_key = FALSE) {
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
  if (!trends) {
    if(missing(levels)) levels <- levels[2:4]
    names(levels) <- strrep(marks[["sig"]], seq_along(levels))
  } else {
    names(levels) <- strrep(marks[["sig"]], seq_along(levels) - 1)
    names(levels)[[1]] <- marks[["trend"]]
  }
  levels <- rev(levels)
  if (is.na(marks[["ns"]])) {
    breaks <- c(-Inf, levels)
    labels <- names(levels)
  } else {
    breaks <- c(-Inf, levels, Inf)
    labels <- c(names(levels), marks[["ns"]])
  }
  out <- cut(p, breaks = breaks, labels = labels, right = FALSE)
  if (include_key) attr(out, "key") <- levels
  out
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

#' Pairwise post-hoc test following Kruskal-Wallis test
#'
#' This is a tidy wrapper around [`dunn.test::dunn.test()`]. It performs Dunn's test, a non-parametric pairwise follow-up for a Kruskal-Wallis test.
#'
#' @param x a numeric vector.
#' @param groups a vector or factor giving the group for the corresponding elements of `x`.
#' @param data a data frame containing the variables.
#' @param p.adjust.method character. method for adjusting p-values for multiple comparisons.
#' @param alpha numeric. alpha level.
#'
#' @return A tibble with columns:
#'   - `contrast`: The compared groups.
#'   - `statistic`: Dunn's test statistic (z).
#'   - `adj.p.value`: Adjusted p-value based on the specified `p.adjust.method`.
#'
#' @seealso [`dunn.test::dunn.test()`]
#'
#' @examples
#' mtcars2 <- transform(mtcars, cyl = factor(cyl))
#' kruskal.test(mpg ~ gear, data = mtcars2)
#' dunn_test(mpg, gear, data = mtcars2)
#'
#' @export
dunn_test <- function(x,
                      groups,
                      data,
                      p.adjust.method = c("holm",
                                          "hochberg",
                                          "bonferroni",
                                          "bh",
                                          "by",
                                          "sidak",
                                          "hs",
                                          "none"),
                      alpha = 0.05) {
  rlang::check_installed("dunn.test")
  p.adjust.method <- match.arg(p.adjust.method)
  x <- dplyr::pull(data, {{x}})
  groups <- dplyr::pull(data, {{groups}})
  invisible(utils::capture.output(
    out <- dunn.test::dunn.test(
      x,
      groups,
      method = p.adjust.method,
      alpha = alpha,
      kw = FALSE
    )
  ))
  with(
    out,
    tibble::tibble(
      contrast = comparisons,
      statistic = Z,
      adj.p.value = P.adjusted
    )
  )
}



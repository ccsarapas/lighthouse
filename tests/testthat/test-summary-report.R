test_that("summary_report autotypes character and factor as nominal", {
  expect_equal(
    summary_report(sr_data, chr),
    tibble::tibble(Variable = "chr", Value = c("a", "b", "c"), V1 = 1, V2 = 1/3)
  )
  expect_equal(
    summary_report(sr_data, fct),
    tibble::tibble(
      Variable = "fct",
      Value = c("lvl 1", "lvl 2"), 
      V1 = c(2, 1), 
      V2 = c(2/3, 1/3)
    )
  )
})

test_that("summary_report autotypes dates and datetimes as nominal", {
  expect_equal(
    summary_report(sr_data, date),
    tibble::tibble(
      Variable = "date",
      Value = c("2022-09-02", "2023-06-15"),
      V1 = c(1, 2),
      V2 = c(1 / 3, 2 / 3),
    )
  )
  expect_equal(
    summary_report(sr_data, POSIXct),
    tibble::tibble(
      Variable = "POSIXct",
      Value = c(
        "2022-02-13 16:12:47", "2023-05-04 11:41:05", "2023-11-23 09:00:07"
      ),
      V1 = 1,
      V2 = 1 / 3,
    )
  )
  expect_equal(
    summary_report(sr_data, POSIXlt),
    tibble::tibble(
      Variable = "POSIXlt",
      Value = c(
        "2023-05-18 12:16:03", "2023-12-01 00:15:00", "2023-12-01 04:15:30"
      ),
      V1 = 1,
      V2 = 1 / 3,
    )
  )
})

test_that("summary_report autotypes logical with missings as nominal", {
  expect_equal(
    summary_report(sr_data, lgl_na),
    tibble::tibble(
      Variable = "lgl_na",
      Value = c("TRUE", NA),
      V1 = c(2, 1),
      V2 = c(2 / 3, 1 / 3),
    )
  )
  # should be binary if na.rm.bin is TRUE, but NOT if na.rm.nom is TRUE
  expect_equal(
    summary_report(sr_data, lgl_all_FALSE_na, na.rm.nom = TRUE),
    tibble::tibble(
      Variable = "lgl_all_FALSE_na",
      Value = "FALSE",
      V1 = 2,
      V2 = 1
    )
  )
})

test_that("summary_report autotypes logical without missings as binary", {
  expect_equal(
    summary_report(sr_data, lgl),
    tibble::tibble(Variable = "lgl", Value = "TRUE", V1 = 2, V2 = 2/3)
  )
  expect_equal(
    summary_report(sr_data, lgl_na, na.rm = TRUE),
    tibble::tibble(Variable = "lgl_na", Value = "TRUE", V1 = 2, V2 = 1)
  )
  expect_equal(
    summary_report(sr_data, lgl_all_FALSE_na, na.rm.bin = TRUE),
    tibble::tibble(
      Variable = "lgl_all_FALSE_na", 
      Value = "TRUE", 
      V1 = 0, 
      V2 = 0
    )
  )
})

test_that("summary_report autotypes other numerics as continuous", {
  expect_equal(
    summary_report(sr_data, num),
    tibble::tibble(
      Variable = "num",
      Value = "mean, sd",
      V1 = mean(sr_data$num),
      V2 = sd(sr_data$num)
    )
  )
})

test_that("summary_report applies default types correctly", {
  expect_equal(
    summary_report(sr_data, num, lgl, .default = "nom"),
    tibble::tibble(
      Variable = rep(c("num", "lgl"), 3:2),
      Value = c("2", "9", "11", "FALSE", "TRUE"),
      V1 = c(rep(1, 4), 2),
      V2 = c(rep(1 / 3, 4), 2 / 3),
    )
  )
  expect_equal(
    summary_report(sr_data, lgl, num_bin, .default = "bin"),
    tibble::tibble(
      Variable = c("lgl", "num_bin"),
      Value = c("TRUE", "1"),
      V1 = c(2, 1),
      V2 = c(2 / 3, 1 / 3),
    )
  )
})

test_that("summary_report applies measurement wrappers correctly", {
  expect_equal(
    summary_report(sr_data, nom(num, lgl)),
    tibble::tibble(
      Variable = rep(c("num", "lgl"), 3:2),
      Value = c("2", "9", "11", "FALSE", "TRUE"),
      V1 = c(rep(1, 4), 2),
      V2 = c(rep(1 / 3, 4), 2 / 3),
    )
  )
  expect_equal(
    summary_report(sr_data, bin(lgl, num_bin)),
    tibble::tibble(
      Variable = c("lgl", "num_bin"),
      Value = c("TRUE", "1"),
      V1 = c(2, 1),
      V2 = c(2 / 3, 1 / 3),
    )
  )
})

test_that("summary_report handle tidyselect expressions", {
  expect_equal(
    summary_report(sr_data, tidyselect::where(is.numeric))$Variable,
    c("num", "num_bin", "num_bin_all_0", "num_two_vals", "num_na")
  )
})

test_that("measurement helpers handle tidyselect expressions", {
  expect_equal(
    summary_report(sr_data, nom(lgl:lgl_all_FALSE, lgl_na)),
    tibble::tibble(
      Variable = c("lgl", "lgl", "lgl_all_FALSE", "lgl_na", "lgl_na"),
      Value = c("FALSE", "TRUE", "FALSE", "TRUE", NA),
      V1 = c(1, 2, 3, 2, 1),
      V2 = c(1/3, 2/3, 1, 2/3, 1/3),
    )
  )
})

test_that("summary_report handles missing values correctly", {
  M_num_na <- mean(sr_data$num_na, na.rm = TRUE)
  SD_num_na <- sd(sr_data$num_na, na.rm = TRUE)
  expect_equal(
    summary_report(sr_data, chr_na, lgl_all_FALSE_na, num_na),
    tibble::tibble(
      Variable = c(
        "chr_na", "chr_na", "chr_na", "lgl_all_FALSE_na", "lgl_all_FALSE_na",
        "num_na"
      ),
      Value = c("a", "c", NA, "FALSE", NA, "mean, sd"),
      V1 = c(1, 1, 1, 2, 1, NA),
      V2 = c(1 / 3, 1 / 3, 1 / 3, 2 / 3, 1 / 3, NA)
    )
  )
  expect_equal(
    summary_report(sr_data, chr_na, lgl_all_FALSE_na, num_na, na.rm = TRUE),
    tibble::tibble(
      Variable = c("chr_na", "chr_na", "lgl_all_FALSE_na", "num_na"),
      Value = c("a", "c", "TRUE", "mean, sd"),
      V1 = c(1, 1, 0, M_num_na),
      V2 = c(1 / 2, 1 / 2, 0, SD_num_na)
    )
  )
  expect_equal(
    summary_report(
      sr_data,
      chr_na, lgl_all_FALSE_na, num_na,
      na.rm.nom = TRUE, na.rm.cont = TRUE
    ),
    tibble::tibble(
      Variable = c("chr_na", "chr_na", "lgl_all_FALSE_na", "num_na"),
      Value = c("a", "c", "FALSE", "mean, sd"),
      V1 = c(1, 1, 2, M_num_na),
      V2 = c(1 / 2, 1 / 2, 1, SD_num_na)
    )
  )
  expect_equal(
    summary_report(
      sr_data,
      chr_na, lgl_all_FALSE_na, num_na,
      na.rm.nom = TRUE, na.rm.cont = TRUE
    ),
    tibble::tibble(
      Variable = c("chr_na", "chr_na", "lgl_all_FALSE_na", "num_na"),
      Value = c("a", "c", "FALSE", "mean, sd"),
      V1 = c(1, 1, 2, M_num_na),
      V2 = c(1 / 2, 1 / 2, 1, SD_num_na)
    )
  )
})

test_that("summary_report has `TRUE` row for binary even if no `TRUE` values", {
  expect_equal(
    summary_report(sr_data, lgl_all_FALSE),
    tibble::tibble(Variable = "lgl_all_FALSE", Value = "TRUE", V1 = 0, V2 = 0)
  )
  expect_equal(
    summary_report(sr_data, bin(num_bin_all_0)),
    tibble::tibble(Variable = "num_bin_all_0", Value = "1", V1 = 0, V2 = 0)
  )
})

test_that("`.drop` argument works correctly", {
  expect_false("empty lvl 3" %in% summary_report(sr_data, fct_empty)$Value)
  expect_in(
    "empty lvl 3",
    summary_report(sr_data, fct_empty, .drop = FALSE)$Value
  )
})

test_that("summary_report uses functions set in .cont_fx", {
    expect_equal(
      summary_report(sr_data, num, .cont_fx = list(median, IQR)),
      tibble::tibble(
        Variable = "num",
        Value = "median, IQR",
        V1 = median(sr_data$num),
        V2 = IQR(sr_data$num)
      )
    )
})

test_that("`.missing_label` labels missing values", {
  expect_contains(
    summary_report(sr_data, chr_na, .missing_label = "(Missing)")$Value,
    "(Missing)"
  )
})

test_that("attempt to set inappropriate continuous variable throws error", {
  expect_error(
    summary_report(sr_data, cont(fct)),
    "set as continuous but is a factor."
  )
  expect_error(
    summary_report(sr_data, cont(date)),
    "set as continuous but is a Date."
  )
})

test_that("attempt to set inappropriate binary variable throws error", {
  expect_error(
    summary_report(sr_data, bin(lgl_all_FALSE_na)),
    "set as binary but has missing values."
  )
  expect_error(
    summary_report(sr_data, bin(num)),
    "supports only logical or binary numeric variables at this time."
  )
  expect_error(
    summary_report(sr_data, bin(num_two_vals)),
    "supports only logical or binary numeric variables at this time."
  )
  expect_error(
    summary_report(sr_data, bin(chr)),
    "supports only logical or binary numeric variables at this time."
  )
})

test_that("summary_report returns consistent column types (#26)", {
  types_if_input <- function(var) {
    sapply(summary_report(sr_data, !!rlang::ensym(var)), typeof)
  }
  types <- c(
    Variable = "character", Value = "character",
    V1 = "double", V2 = "double"
  )
  expect_equal(types_if_input(chr), types)
  expect_equal(types_if_input(fct), types)
  expect_equal(types_if_input(num), types)
  expect_equal(types_if_input(lgl), types)
  expect_equal(types_if_input(date), types)
  expect_equal(types_if_input(POSIXct), types)
  expect_equal(types_if_input(POSIXlt), types)
})

test_that("setting .`missing_label` doesn't throw error (#26)", {
  expect_no_error(summary_report(sr_data, chr_na, .missing_label = "(Missing)"))
})

test_that("summary_report doesn't cause deprecation warning (#19)", {
  expect_no_warning(
    summary_report(sr_data, chr, fct),
    class = "lifecycle_warning_deprecated"
  )
  expect_no_warning(
    summary_report(sr_data, nom(chr, fct)),
    class = "lifecycle_warning_deprecated"
  )
})

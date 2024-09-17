test_that("summary_table applies standard functions", {
  expect_equal(
    summary_table(mtcars, mean, .vars = mpg),
    tibble::tibble(Variable = "mpg", mean = mean(mtcars$mpg))
  )
})

### rmv `...` after adding `try.na.rm`
test_that("summary_table applies anonymous functions", {
  expect_equal(
    summary_table(mtcars, \(x, ...) sd(x) / sqrt(length(x)), .vars = mpg),
    tibble::tibble(
      Variable = "mpg", 
      `function(x, ...) sd(x)/sqrt(length(x))` = sd(mtcars$mpg) / sqrt(length(mtcars$mpg))
    )
  )
})

### rmv `...` after adding `try.na.rm`
test_that("summary_table takes function names", {
  expect_equal(
    summary_table(
      mtcars,
      M = mean, SEM = \(x, ...) sd(x) / sqrt(length(x)),
      .vars = mpg
    ),
    tibble::tibble(
      Variable = "mpg",
      M = mean(mtcars$mpg),
      SEM = sd(mtcars$mpg) / sqrt(length(mtcars$mpg))
    )
  )
})

test_that("summary_table passes `na.rm` to functions", {
  expect_equal(summary_table(sr_data, mean, .vars = num_na)$mean, NA_real_)
  expect_equal(
    summary_table(sr_data, mean, .vars = num_na, na.rm = TRUE)$mean, 
    mean(sr_data$num_na, na.rm = TRUE)
  )
})

test_that("summary_table handles functions that do not accept `na.rm`", {
  expect_no_error(summary_table(mtcars, length, .vars = mpg))
  expect_no_error(summary_table(mtcars, length, .vars = mpg, na.rm = TRUE))
  expect_equal(
    summary_table(mtcars, length, .vars = mpg)$length,
    length(mtcars$mpg)
  )
  expect_no_error(
    summary_table(mtcars, length, .vars = mpg, na.rm = TRUE)$length,
    length(mtcars$mpg)
  )
})

test_that("`.vars` handles tidyselect expressions", {
  expect_equal(
    summary_table(sr_data, mean, .vars = num:num_bin_all_0)$Variable,
    c("num", "num_bin", "num_bin_all_0")
  )
  expect_equal(
    summary_table(sr_data, mean, .vars = where(is.numeric))$Variable,
    c("num", "num_bin", "num_bin_all_0", "num_two_vals", "num_na")
  )
  vars <- c("chr", "fct", "num", "lgl")
  expect_equal(
    summary_table(sr_data, n_valid, .vars = all_of(vars))$Variable,
    vars
  )
  expect_equal(
    summary_table(sr_data, n_valid, .vars = c(chr:fct, chr_na:lgl_na))$Variable,
    c("chr", "fct", "chr_na", "num_na", "lgl_na")
  )
  expect_equal(
    summary_table(
      sr_data, 
      n_valid, 
      .vars = c(dbl = num, dbl_na = num_na)
    )$Variable,
    c("dbl", "dbl_na")
  )
})

test_that("summary_table groups rows when `.rows_group_by` is specified", {
  expect_equal(
    summary_table(mtcars2, median, .vars = mpg, .rows_group_by = trans),
    tibble::tibble(
      trans = c("auto", "manual"),
      Variable = c("mpg", "mpg"),
      median = c(17.3, 22.8)
    )
  )
  expect_equal(
    summary_table(mtcars2, median, .vars = mpg, .rows_group_by = c(trans, cyl)),
    tibble::tibble(
      trans = rep(c("auto", "manual"), each = 3),
      cyl = rep(c(4, 6, 8), 2),
      Variable = "mpg",
      median = c(22.8, 18.65, 15.2, 28.85, 21, 15.4)
    )
  )
})

test_that("summary_table groups rows when `.data` is grouped", {
  expect_equal(
    mtcars2 %>% 
    dplyr::group_by(trans, cyl) %>% 
    summary_table(median, .vars = mpg),
    tibble::tibble(
      trans = rep(c("auto", "manual"), each = 3),
      cyl = rep(c(4, 6, 8), 2),
      Variable = "mpg",
      median = c(22.8, 18.65, 15.2, 28.85, 21, 15.4)
    )
  )
  expect_equal(
    mtcars2 %>% 
    dplyr::group_by(trans) %>% 
    summary_table(median, .vars = mpg, .rows_group_by = cyl),
    tibble::tibble(
      trans = rep(c("auto", "manual"), each = 3),
      cyl = rep(c(4, 6, 8), 2),
      Variable = "mpg",
      median = c(22.8, 18.65, 15.2, 28.85, 21, 15.4)
    )
  )
})

test_that("summary_table groups columns when `.cols_group_by` is specified", {
  expect_equal(
    summary_table(mtcars2, median, .vars = mpg, .cols_group_by = c(trans, cyl)),
    tibble::tibble(
      Variable = "mpg",
      auto_4 = 22.8, auto_6 = 18.65, auto_8 = 15.2,
      manual_4 = 28.85, manual_6 = 21, manual_8 = 15.4
    )
  )
  expect_equal(
    summary_table(
      mtcars2, 
      min, max, 
      .vars = mpg, 
      .cols_group_by = c(trans, cyl)
    ),
    tibble::tibble(
      Variable = "mpg",
      min_auto_4 = 21.5, max_auto_4 = 24.4,
      min_auto_6 = 17.8, max_auto_6 = 21.4,
      min_auto_8 = 10.4, max_auto_8 = 19.2,
      min_manual_4 = 21.4, max_manual_4 = 33.9,
      min_manual_6 = 19.7, max_manual_6 = 21,
      min_manual_8 = 15, max_manual_8 = 15.8
    )
  )
})

test_that("summary_table passes `.cols_group_opts` to pivot_wider", {
  expect_equal(
    summary_table(
      mtcars2,
      min, max,
      .vars = c(mpg, hp),
      .cols_group_by = c(trans, cyl),
      .cols_group_opts = list(names_glue = "{cyl} cyl {trans}: {.value}")
    ),
    tibble::tibble(
      Variable = c("mpg", "hp"),
      `4 cyl auto: min` = c(21.5, 62),
      `4 cyl auto: max` = c(24.4, 97),
      `6 cyl auto: min` = c(17.8, 105),
      `6 cyl auto: max` = c(21.4, 123),
      `8 cyl auto: min` = c(10.4, 150),
      `8 cyl auto: max` = c(19.2, 245),
      `4 cyl manual: min` = c(21.4, 52),
      `4 cyl manual: max` = c(33.9, 113),
      `6 cyl manual: min` = c(19.7, 110),
      `6 cyl manual: max` = c(21, 175),
      `8 cyl manual: min` = c(15, 264),
      `8 cyl manual: max` = c(15.8, 335),
    )
  )
})

test_that("summary_table handles `.var_col_name` correctly", {
  expect_equal(
    names(summary_table(mtcars, median, .vars = mpg))[[1]],
    "Variable"
  )
  expect_equal(
    names(
      summary_table(mtcars, median, .vars = mpg, .var_col_name = "Outcome")
    )[[1]],
    "Outcome"
  )
  expect_equal(
    ncol(summary_table(mtcars, median, .vars = mpg, .var_col_name = NULL)),
    1L
  )
  expect_equal(
    names(
      summary_table(
        mtcars,
        median,
        .vars = c(mpg, hp),
        .var_col_name = "Outcome"
      )
    )[[1]],
    "Outcome"
  )
  expect_error(
    summary_table(mtcars, median, .vars = c(mpg, hp), .var_col_name = NULL)
  )
})


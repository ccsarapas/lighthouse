test_that("try.na.rm works if `fn` takes `na.rm`", {
  x <- c(1, 2, 3, 5, 7, NA)
  expect_equal(try.na.rm(sd, x), NA_real_)
  expect_equal(try.na.rm(sd, x, na.rm = FALSE), NA_real_)
  expect_equal(try.na.rm(sd, x, na.rm = TRUE), sd(x, na.rm = TRUE))
})

test_that("try.na.rm works if `fn` takes `na.rm` via `...`", {
  x <- c(1, 2, 3, 5, 7, NA)
  expect_equal(try.na.rm(mean, x), NA_real_)
  expect_equal(try.na.rm(mean, x, na.rm = FALSE), NA_real_)
  expect_equal(try.na.rm(mean, x, na.rm = TRUE), mean(x, na.rm = TRUE))
})

test_that("try.na.rm works when `fn` doesn't take `na.rm` or `...`)", {
  x <- c(1, 2, 3, 5, 7, NA)
  expect_error(length(x, na.rm = TRUE))
  expect_no_error(try.na.rm(length, x, na.rm = TRUE))
  expect_equal(try.na.rm(length, x), length(x))
  expect_equal(try.na.rm(length, x, na.rm = FALSE), length(x))
  expect_equal(try.na.rm(length, x, na.rm = TRUE), length(x))
})

test_that("try.na.rm handles other arguments via `...`", {
  x <- c(1, 2, 3, 5, 7, NA)
  expect_equal(
    try.na.rm(mean, x, na.rm = TRUE, trim = 0.25),
    mean(x, na.rm = TRUE, trim = 0.25)
  )
  expect_error(
    log(x, na.rm = TRUE, base = 10)
  )
  expect_equal(
    try.na.rm(log, x, na.rm = TRUE, base = 10),
    log(x, base = 10)
  )
})

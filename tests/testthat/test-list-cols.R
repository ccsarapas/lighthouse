test_that("handle_list_cols leaves ordinary data frames unchanged", {
  plain <- data.frame(id = 1:2, value = c(10, 20))

  expect_identical(handle_list_cols(plain, caller = "test"), plain)
})

test_that("handle_list_cols removes list columns with an optional warning", {
  x <- tibble::tibble(meta = list("a", "b"), id = 1:2, value = c(10, 20))
  expected <- tibble::tibble(id = 1:2, value = c(10, 20))

  expect_warning(result <- handle_list_cols(x, caller = "test"), "List column")
  expect_identical(result, expected)
  expect_no_warning(quiet_result <- handle_list_cols(x, quiet = TRUE))
  expect_identical(quiet_result, expected)
})

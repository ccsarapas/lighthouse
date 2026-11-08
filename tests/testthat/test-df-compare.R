test_that("df_compare compares ordinary tibbles", {
  x <- tibble::tibble(id = 1:2, value = c(10, 20))
  y <- tibble::tibble(id = 1:2, value = c(10, 30))

  expect_equal(
    df_compare(x, y, keep = id),
    tibble::tibble(id = 2L, value.x = 20, value.y = 30)
  )
})

test_that("df_compare retains the correct differences after removing list columns", {
  # A leading list column must not shift the names of the compared columns.
  x <- tibble::tibble(meta = list("a", "b"), id = 1:2, value = c(10, 20))
  y <- tibble::tibble(meta = list("a", "b"), id = 1:2, value = c(10, 30))

  expect_warning(result <- df_compare(x, y, keep = id), "List column")
  expect_equal(result, tibble::tibble(id = 2L, value.x = 20, value.y = 30))
})

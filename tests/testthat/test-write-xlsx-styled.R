test_that("write_xlsx_styled exports ordinary data frames", {
  skip_if_not_installed("openxlsx")
  x <- data.frame(id = 1:2, value = c(10, 20))
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  write_xlsx_styled(x, path)

  expect_true(file.exists(path))
  expect_equal(openxlsx::read.xlsx(path), x)
})

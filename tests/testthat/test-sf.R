sf_test_data <- function() {
  skip_if_not_installed("sf")
  sf::st_as_sf(
    tibble::tibble(
      group = c("a", "a", "b", "b"),
      value = c(2, 2, 4, NA_real_),
      x = 1:4,
      y = 1:4
    ),
    coords = c("x", "y")
  )
}

test_that("handle_sf converts sf inputs and leaves ordinary inputs unchanged", {
  spatial <- sf_test_data()
  tabular <- tibble::as_tibble(spatial)

  expect_equal(handle_sf(spatial, quiet = TRUE), tabular)
  expect_no_message(handle_sf(spatial, quiet = TRUE))
  expect_identical(handle_sf(tabular), tabular)
  plain <- data.frame(value = 1:3)
  expect_identical(handle_sf(plain), plain)
})

test_that("counting functions give tabular results for sf inputs", {
  spatial <- sf_test_data()
  tabular <- tibble::as_tibble(spatial)
  counters <- list(
    total = function(x) count_with_total(x, group),
    percent = function(x) count_pct(x, value, .by = group),
    multiple = function(x) count_multiple(x, value, .by = group),
    duplicates = function(x) count_duplicates(x, value),
    unique = function(x) count_unique(x, value),
    missing = function(x) count_na(x, group, value),
    crosstab = function(x) crosstab(x, group, value)
  )

  for (counter in counters) {
    expect_equal(suppressMessages(counter(spatial)), counter(tabular))
  }
})

test_that("summary functions give tabular results for sf inputs", {
  spatial <- sf_test_data()
  tabular <- tibble::as_tibble(spatial)

  expect_equal(
    suppressMessages(summary_table(
      spatial, mean, .vars = value, .rows_group_by = group, na.rm = TRUE
    )),
    summary_table(
      tabular, mean, .vars = value, .rows_group_by = group, na.rm = TRUE
    )
  )
  expect_equal(
    suppressMessages(summary_report(spatial, group, cont(value), na.rm = TRUE)),
    summary_report(tabular, group, cont(value), na.rm = TRUE)
  )
})

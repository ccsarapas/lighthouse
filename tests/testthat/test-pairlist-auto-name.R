test_that("pairlist_auto_name works with standard functions", {
  expect_equal(
    pairlist_auto_name(m = mean, sd),
    list(m = mean, sd = sd)
  )
})

test_that("pairlist_auto_name works with anonymous functions", {
  expect_equal(
    pairlist_auto_name(double = function(x) x * 2, function(x) x * 3),
    list(double = function(x) x * 2, `function(x) x * 3` = function(x) x * 3)
  )
  expect_equal(
    pairlist_auto_name(double = \(x) x * 2, \(x) x * 3),
    list(double = \(x) x * 2, `function(x) x * 3` = \(x) x * 3)
  )
})

test_that("pairlist_auto_name works with old purrr-style anonymous functions", {
  expect_equal(
    pairlist_auto_name(double = ~ .x * 2, ~ .x * 3),
    list(double = ~.x * 2, `~.x * 3` = ~.x * 3)
  )
})

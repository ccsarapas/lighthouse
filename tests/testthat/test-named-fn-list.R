test_that("named_fn_list works with unnamed args", {
  expect_equal(
    named_fn_list(mean, sd, \(x) x * 2),
    list(mean = mean, sd = sd, `function(x) x * 2` = \(x) x * 2)
  )
})

test_that("named_fn_list works with named and unnamed args", {
  expect_equal(
    named_fn_list(m = mean, sd, double = \(x) x * 2),
    list(m = mean, sd = sd, double = \(x) x * 2)
  )
})


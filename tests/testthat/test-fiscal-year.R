library(testthat)
library(lubridate)

test_that("fiscal_year returns correct fiscal year with default fiscal_start", {
  expect_equal(fiscal_year(as.Date("2023-01-01")), 2023)
  expect_equal(fiscal_year(as.Date("2023-12-31")), 2023)
  expect_equal(fiscal_year(as.Date("2022-12-31")), 2022)
})

test_that("fiscal_year returns correct fiscal year with custom fiscal_start", {
  expect_equal(fiscal_year(as.Date("2023-06-30"), fiscal_start = 7), 2023)
  expect_equal(fiscal_year(as.Date("2023-07-01"), fiscal_start = 7), 2024)
  expect_equal(fiscal_year(as.Date("2023-06-30"), fiscal_start = 10), 2023)
  expect_equal(fiscal_year(as.Date("2023-10-01"), fiscal_start = 10), 2024)
})

test_that("fiscal_year returns correct date_first and date_last", {
  expect_equal(fiscal_year(as.Date("2023-05-15"), type = "date_first"),
               as.Date("2023-01-01"))
  expect_equal(fiscal_year(as.Date("2023-05-15"), type = "date_last"),
               as.Date("2023-12-31"))
  expect_equal(fiscal_year(as.Date("2023-05-15"), type = "date_first", fiscal_start = 7),
               as.Date("2022-07-01"))
  expect_equal(fiscal_year(as.Date("2023-05-15"), type = "date_last", fiscal_start = 7),
               as.Date("2023-06-30"))
})

test_that("ffy returns correct federal fiscal year", {
  expect_equal(ffy(as.Date("2023-09-30")), 2023)
  expect_equal(ffy(as.Date("2023-10-01")), 2024)
  expect_equal(
    ffy(as.Date("2023-09-30"), type = "date_first"),
    as.Date("2022-10-01")
  )
  expect_equal(
    ffy(as.Date("2023-09-30"), type = "date_last"),
    as.Date("2023-09-30")
  )
  expect_equal(
    ffy(as.Date("2023-10-01"), type = "date_first"),
    as.Date("2023-10-01")
  )
  expect_equal(
    ffy(as.Date("2023-10-01"), type = "date_last"),
    as.Date("2024-09-30")
  )
})

test_that("sfy_il returns correct Illinois fiscal year", {
  expect_equal(sfy_il(as.Date("2023-06-30")), 2023)
  expect_equal(sfy_il(as.Date("2023-07-01")), 2024)
  expect_equal(
    sfy_il(as.Date("2023-06-30"), type = "date_first"),
    as.Date("2022-07-01")
  )
  expect_equal(
    sfy_il(as.Date("2023-06-30"), type = "date_last"),
    as.Date("2023-06-30")
  )
  expect_equal(
    sfy_il(as.Date("2023-07-01"), type = "date_first"),
    as.Date("2023-07-01")
  )
  expect_equal(
    sfy_il(as.Date("2023-07-01"), type = "date_last"),
    as.Date("2024-06-30")
  )  
})

test_that("ffyq returns correct federal fiscal quarter", {
  expect_equal(ffyq(as.Date("2023-09-30")), 2023.4)
  expect_equal(ffyq(as.Date("2023-10-01")), 2024.1)
  expect_equal(ffyq(as.Date("2023-12-31"), type = "quarter"), 1)
  expect_equal(
    ffyq(as.Date("2023-12-31"), type = "date_first"),
    as.Date("2023-10-01")
  )
  expect_equal(
    ffyq(as.Date("2023-12-31"), type = "date_last"),
    as.Date("2023-12-31")
  )
})

test_that("sfyq_il returns correct Illinois fiscal quarter", {
  expect_equal(sfyq_il(as.Date("2023-06-30")), 2023.4)
  expect_equal(sfyq_il(as.Date("2023-07-01")), 2024.1)
  expect_equal(sfyq_il(as.Date("2023-09-30"), type = "quarter"), 1)
  expect_equal(
    sfyq_il(as.Date("2023-09-30"), type = "date_first"),
    as.Date("2023-07-01")
  )
  expect_equal(
    sfyq_il(as.Date("2023-09-30"), type = "date_last"),
    as.Date("2023-09-30")
  )
})

test_that("functions handle POSIXct date-times", {
  datetime <- as.POSIXct("2023-08-15 12:34:56")
  expect_equal(fiscal_year(datetime), 2023)
  expect_equal(ffy(datetime), 2023)
  expect_equal(sfy_il(datetime), 2024)
  expect_equal(ffyq(datetime), 2023.4)
  expect_equal(sfyq_il(datetime), 2024.1)
})

test_that("functions handle vector inputs correctly (#23)", {
  dates <- as.Date(c("2023-02-01", "2023-07-01", "2023-12-25"))
  expect_equal(fiscal_year(dates, fiscal_start = 7), c(2023, 2024, 2024))
  expect_equal(ffy(dates), c(2023, 2023, 2024))
  expect_equal(sfy_il(dates), c(2023, 2024, 2024))
  expect_equal(ffyq(dates), c(2023.2, 2023.4, 2024.1))
  expect_equal(sfyq_il(dates), c(2023.3, 2024.1, 2024.2))

  dts <- as.POSIXct(c("2023-02-01 12:34:56", "2023-07-01 12:34:56", "2023-12-25 12:34:56"))
  expect_equal(fiscal_year(dts, fiscal_start = 7), c(2023, 2024, 2024))
  expect_equal(ffy(dts), c(2023, 2023, 2024))
  expect_equal(sfy_il(dts), c(2023, 2024, 2024))
  expect_equal(ffyq(dts), c(2023.2, 2023.4, 2024.1))
  expect_equal(sfyq_il(dts), c(2023.3, 2024.1, 2024.2))
})

test_that("functions handle leap years correctly", {
  dt <- as.POSIXct("2020-02-29 12:34:56")
  expect_equal(fiscal_year(dt), 2020)
  expect_equal(fiscal_year(dt, fiscal_start = 4), 2020)
  expect_equal(ffy(dt), 2020)
  expect_equal(sfy_il(dt), 2020)
  expect_equal(ffyq(dt), 2020.2)
  expect_equal(sfyq_il(dt), 2020.3)
})

test_that("functions validate `type` parameter", {
  expect_error(fiscal_year(as.Date("2023-01-01"), type = "invalid"))
  expect_error(ffyq(as.Date("2023-01-01"), type = "invalid"))
})

test_that("functions handle NA inputs", {
  expect_equal(fiscal_year(NA), NA_real_)
  expect_equal(fiscal_year(NA, type = "date_first"), as.Date(NA))
  expect_equal(sfyq_il(NA), NA_real_)
  expect_equal(sfyq_il(NA, type = "date_first"), as.Date(NA))
})

test_that("functions respect time zones", {
  # FY 2024 in UTC but still FY 2023 in NY
  dt_ny <- as.POSIXct("2023-09-30 20:00:00", tz = "America/New_York")
  expect_equal(ffy(dt_ny), 2023)
  expect_equal(ffyq(dt_ny), 2023.4)
})

test_that("fiscal_year validates `fiscal_start`", {
  d <- as.Date("2023-05-20")
  msg <- "`fiscal_start` must be a single integer between 1 and 12"
  expect_error(fiscal_year(d, fiscal_start = 0), msg)
  expect_error(fiscal_year(d, fiscal_start = 1.5), msg)
  expect_error(fiscal_year(d, fiscal_start = 13), msg)
  expect_error(fiscal_year(d, fiscal_start = c(1, 4, 7)), msg)
})


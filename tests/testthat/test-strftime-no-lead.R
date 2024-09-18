test_that("strftime_no_lead works correctly with default parameters (#21)", {
  expect_equal(strftime_no_lead(as.Date("2023-06-05")), "2023-6-5")
  expect_equal(
    strftime_no_lead(as.POSIXct("2023-06-05 01:02:03")),
    "2023-6-5 1:02:03"
  )
})

test_that("strftime_no_lead works correctly with custom date strings", {
  expect_equal(
    strftime_no_lead(as.Date("2023-06-05"), "%B %d, %Y"), 
    "June 5, 2023"
  )
  expect_equal(
    strftime_no_lead(as.POSIXct("2023-06-05 01:02:03"), "%m/%d/%Y %H:%M:%OS3"), 
    "6/5/2023 1:02:03.000"
  )
})


test_that("strftime_no_lead works with custom no_lead components", {
  expect_equal(
    strftime_no_lead(as.Date("2023-06-05"), "%m/%d/%Y", no_lead = "%m"), 
    "6/05/2023"
  )
  expect_equal(
    strftime_no_lead(
      as.POSIXct("2023-06-05 01:02:03.456"), 
      "%Hh %Mm %OS2s", 
      no_lead = c("%H", "%M", "%OS2")
    ), 
    "1h 2m 3.45s"
  )
})

test_that("strftime_no_lead works with hms objects", {
  expect_equal(
    strftime_no_lead(
      hms::as_hms("01:02:03.456"), 
      "%Hh %Mm %OS1s", 
      no_lead = c("%H", "%M", "%OS")
    ), 
    "1h 2m 3.4s"
  )
})

test_that("strftime_no_lead behaves like strftime when no_lead is NULL", {
  dt <- as.POSIXct("2023-06-05 01:02:03")
  expect_equal(
    strftime_no_lead(dt, "%m/%d/%Y %H:%M:%S", no_lead = NULL), 
    strftime(dt, "%m/%d/%Y %H:%M:%S")
  )
})

test_that("strftime_no_lead handles time zones correctly", {
  dt <- as.POSIXct("2023-06-05 01:02:03", tz = "UTC")
  expect_match(
    strftime_no_lead(dt, "%m/%d/%y %H:%M %Z", tz = "America/New_York"),
    "6/4/23 21:02 EDT"
  )
  expect_match(
    strftime_no_lead(dt, tz = "Pacific/Auckland", usetz = TRUE), 
    "2023-6-5 13:02:03 NZST"
  )
})

test_that("strftime_no_lead handles vectorized inputs correctly", {
  d <- as.Date(c("2023-06-05", "2023-12-25"))
  expect_equal(strftime_no_lead(d, "%m/%d/%Y"), c("6/5/2023", "12/25/2023"))
  dt <- as.POSIXct(c("2023-06-05 01:02:03", "2023-12-25 11:22:33"))
  expect_equal(
    strftime_no_lead(dt, "%m/%d/%Y %H:%M:%S"), 
    c("6/5/2023 1:02:03", "12/25/2023 11:22:33")
  )
})

test_that("strftime_no_lead does not remove non-leading zeroes", {
  expect_equal(
    strftime_no_lead(as.POSIXct("2000-01-10 10:00:00"), "%m/%d/%Y %H:%M:%OS2"), 
    "1/10/2000 10:00:00.00"
  )
})

test_that("strftime_no_lead handles invalid inputs gracefully", {
  expect_error(
    strftime_no_lead("not a date"), 
    "character string is not in a standard unambiguous format"
  )
})


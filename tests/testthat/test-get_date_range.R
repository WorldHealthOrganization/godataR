test_that("get_date_range works as expected", {
  x <- c("2020-07-15", "2021-08-09", "2022-08-03")
  date_range <- get_date_range(dates = x)

  expect_type(date_range, "list")
  expect_named(date_range, c("mindate", "maxdate"))
  expect_identical(
    date_range,
    list(
      mindate = as.POSIXct("2020-07-15", tz = "UTC"),
      maxdate = as.POSIXct("2022-08-03", tz = "UTC")
    )
  )
})

test_that("get_date_range works as expected with missing values", {
  x <- c("2020-07-15", NA, "2021-08-09", "2022-08-03", NA)
  date_range <- get_date_range(dates = x)

  expect_type(date_range, "list")
  expect_named(date_range, c("mindate", "maxdate"))
  expect_identical(
    date_range,
    list(
      mindate = as.POSIXct("2020-07-15", tz = "UTC"),
      maxdate = as.POSIXct("2022-08-03", tz = "UTC")
    )
  )
})

test_that("get_date_range works as expected with different formats", {
  # first 3 are ymd, last is dmy
  x <- c("2020-07-15", "2021-08-09", "2022-08-03", "13-04-2023")
  date_range <- get_date_range(dates = x)

  expect_type(date_range, "list")
  expect_named(date_range, c("mindate", "maxdate"))
  expect_identical(
    date_range,
    list(
      mindate = as.POSIXct("2020-07-15", tz = "UTC"),
      maxdate = as.POSIXct("2023-04-13", tz = "UTC")
    )
  )
})

test_that("get_date_range cannot handle ambiguous mixed formats", {
  # last two are dmy and mdy, so max date should be the last one
  x <- c("2020-07-15", "2021-08-09", "2022-08-03", "01-04-2023", "05-01-2023")
  date_range <- get_date_range(dates = x)

  expect_type(date_range, "list")
  expect_named(date_range, c("mindate", "maxdate"))
  expect_identical(
    date_range,
    list(
      mindate = as.POSIXct("2020-07-15", tz = "UTC"),
      maxdate = as.POSIXct("2023-04-01", tz = "UTC")
    )
  )
})

test_that("get_date_range throws warning with incorrect dates", {
  x <- c("2020-07-15", "2021-08-09", "2022-08-03", "2023-13-01")
  expect_warning(date_range <- get_date_range(dates = x))

  expect_type(date_range, "list")
  expect_named(date_range, c("mindate", "maxdate"))
  expect_identical(
    date_range,
    list(
      mindate = as.POSIXct("2020-07-15", tz = "UTC"),
      maxdate = as.POSIXct("2022-08-03", tz = "UTC")
    )
  )
})


test_that("mongify_date works as expected", {

  dates <- c("08/01/2022", "08/31/2022")
  converted_dates <- mongify_date(dates = dates, dateformat = "mdy")

  expect_type(converted_dates, "character")
  expect_length(converted_dates, length(dates))
  expect_true(
    all(grepl(
      pattern = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}.\\d{3}Z",
      x = converted_dates
    ))
  )
})

test_that("mongify_date works as expected with undefined format", {

  dates <- c("08/01/2022", "08/31/2022")
  converted_dates <- mongify_date(dates = dates, dateformat = "undefined")

  expect_type(converted_dates, "character")
  expect_length(converted_dates, length(dates))
  expect_true(
    all(grepl(
      pattern = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}.\\d{3}Z",
      x = converted_dates
    ))
  )
})

test_that("mongify_date works as expected with ymd format", {

  dates <- c("2022/08/01", "2022/08/31")
  converted_dates <- mongify_date(dates = dates, dateformat = "ymd")

  expect_type(converted_dates, "character")
  expect_length(converted_dates, length(dates))
  expect_true(
    all(grepl(
      pattern = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}.\\d{3}Z",
      x = converted_dates
    ))
  )
})

test_that("mongify_date throws warning with incorrect format", {

  # second date cannot be dmy format
  dates <- c("08/01/2022", "08/31/2022")
  expect_warning(
    converted_dates <- mongify_date(dates = dates, dateformat = "dmy"),
    regexp = "1 failed to parse."
  )

  expect_type(converted_dates, "character")
  expect_true(anyNA(converted_dates))
  expect_length(converted_dates, length(dates))
  # conforms to expected format or is NA
  expect_true(
    all(grepl(
      pattern = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}.\\d{3}Z",
      x = converted_dates
    ) | is.na(converted_dates)
    )
  )
})

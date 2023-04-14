test_that("clean_events works as expected", {
  skip("get_events requires API call")

  events <- get_events(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )

  locations <- get_locations(
    url = url,
    username = username,
    password = password
  )
  locations_clean <- clean_locations(locations = locations)

  res <- clean_events(
    events = events,
    locations_clean = locations_clean
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(3L, 21L))
  expect_true(
    all(c(
      "id", "name", "date", "date_of_reporting", "description",
      "responsible_user", "admin_0_name", "admin_1_name", "admin_2_name", "lat",
      "long", "address", "postal_code", "city", "telephone", "email",
      "location_id", "created_by", "datetime_created_at", "updated_by",
      "datetime_updated_at"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], function(x) class(x)[1])),
    c(
      "character", "character", "character", "Date", "logical", "logical",
      "character", "character", "character", "numeric", "numeric", "logical",
      "logical", "logical", "logical", "logical", "character", "character",
      "POSIXct", "character", "POSIXct"
    )
  )
})

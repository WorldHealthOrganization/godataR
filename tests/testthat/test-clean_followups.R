test_that("clean_followups works as expected", {
  skip("get_followups requires API call")

  followups <- get_followups(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )

  contacts <- get_contacts(
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

  contacts_address_history_clean <- clean_contact_address_history(
    contacts = contacts,
    locations_clean = locations_clean
  )

  res <- clean_followups(
    followups = followups,
    contacts_address_history_clean = contacts_address_history_clean
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(49L, 24L))
  expect_true(
    all(c(
      "id", "contact_id", "contact_visual_id", "date", "followup_number",
      "followup_status", "targeted", "responsible_user_id", "team_id",
      "admin_0_name", "admin_1_name", "admin_2_name", "lat", "long", "address",
      "postal_code", "city", "telephone", "email", "location_id", "created_by",
      "datetime_created_at", "updated_by", "datetime_updated_at"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], function(x) class(x)[1])),
    c(
      "character", "character", "character", "Date", "integer", "character",
      "logical", "character", "character", "character", "character",
      "character", "numeric", "numeric", "character", "logical", "character",
      "character", "character", "character", "character", "POSIXct",
      "character", "POSIXct"
    )
  )
})

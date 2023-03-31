test_that("cases_from_contacts works as expected", {
  skip("get_cases requires API call")

  cases <- get_cases(
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

  cases_vacc_history_clean <- clean_case_vax_history(cases = cases)
  cases_address_history_clean <- clean_case_address_history(
    cases = cases,
    locations_clean = locations_clean
  )
  cases_dateranges_history_clean <- clean_case_med_history(cases = cases)

  cases_clean <- clean_cases(
    cases = cases,
    cases_address_history_clean = cases_address_history_clean,
    cases_vacc_history_clean = cases_vacc_history_clean,
    cases_dateranges_history_clean = cases_dateranges_history_clean
  )

  res <- cases_from_contacts(cases_clean = cases_clean)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(3L, 43L))
  expect_true(
    all(c(
      "id", "visual_id", "classification", "follow_up_status", "first_name",
      "middle_name", "last_name", "gender", "age", "age_class", "occupation",
      "pregnancy_status", "date_of_reporting", "date_of_last_contact",
      "date_of_burial", "risk_level", "risk_reason", "responsible_user_id",
      "follow_up_team_id", "admin_0_name", "admin_1_name", "admin_2_name",
      "lat", "long", "address", "postal_code", "city", "telephone", "email",
      "vaccinated", "outcome", "date_of_outcome", "relationship_exposure_type",
      "relationship_context_of_transmission", "relationship_exposure_duration",
      "relationship_exposure_frequency", "relationship_certainty_level",
      "relationship_cluster_id", "location_id", "created_by",
      "datetime_created_at", "updated_by", "datetime_updated_at"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], function(x) class(x)[1])),
    c(
      rep("character", 8), "numeric", "factor", "character", "character",
      "Date", "logical", "Date", "character", "character", "character",
      "logical", "character", "character", "character", "numeric", "numeric",
      rep("character", 5), "logical", "character", "Date", rep("logical", 6),
      "character", "character", "POSIXct", "character", "POSIXct"
    )
  )
})

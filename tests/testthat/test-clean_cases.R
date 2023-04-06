test_that("clean_cases works as expected", {
  skip("get_cases requires API call")

  cases <- get_cases(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )

  language_tokens <- get_language_tokens(
    url = url,
    username = username,
    password = password,
    language = "english_us"
  )

  locations <- get_locations(
    url = url,
    username = username,
    password = password
  )

  locations_clean <- clean_locations(locations = locations)

  cases_vacc_history_clean <- clean_case_vax_history(
    cases = cases,
    language_tokens = language_tokens
  )
  cases_address_history_clean <- clean_case_address_history(
    cases = cases,
    locations_clean = locations_clean,
    language_tokens = language_tokens
  )
  cases_dateranges_history_clean <- clean_case_med_history(
    cases = cases,
    language_tokens = language_tokens
  )

  res <- clean_cases(
    cases = cases,
    cases_address_history_clean = cases_address_history_clean,
    cases_vacc_history_clean = cases_vacc_history_clean,
    cases_dateranges_history_clean = cases_dateranges_history_clean,
    language_tokens = language_tokens
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(14L, 43L))
  expect_true(
    all(c(
      "id", "visual_id", "classification", "first_name", "middle_name",
      "last_name", "gender", "age", "age_class", "occupation",
      "pregnancy_status", "date_of_reporting", "date_of_onset",
      "date_of_infection", "date_become_case", "date_of_burial", "was_contact",
      "risk_level", "risk_reason", "safe_burial", "transfer_refused",
      "responsible_user_id", "admin_0_name", "admin_1_name", "admin_2_name",
      "lat", "long", "address", "postal_code", "city", "telephone", "email",
      "vaccinated", "isolated", "hospitalized", "icu", "outcome",
      "date_of_outcome", "location_id", "created_by", "datetime_created_at",
      "updated_by", "datetime_updated_at"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], function(x) class(x)[1])),
    c(
      rep("character", 7), "numeric", "factor", "character", "character",
      rep("Date", 5), "logical", "character", "character", "logical", "logical",
      "character", "character", "character", "character", "numeric", "numeric",
      "character", "character", "character", "character", "character",
      "logical", "logical", "logical", "logical", "character", "Date",
      "character", "character", "POSIXct", "character", "POSIXct"
    )
  )
})

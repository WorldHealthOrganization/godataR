test_that("clean_contacts works as expected", {
  skip("get_contacts requires API call")

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

  language_tokens <- get_language_tokens(
    url = url,
    username = username,
    password = password,
    language = "english_us"
  )

  # other cleaned data required for `clean_contacts()`
  contacts_vacc_history_clean <- clean_contact_vax_history(
    contacts = contacts,
    language_tokens = language_tokens
  )
  contacts_address_history_clean <- clean_contact_address_history(
    contacts = contacts,
    locations_clean = locations_clean,
    language_tokens = language_tokens
  )

  cases <- get_cases(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )
  cases_address_history_clean <- clean_case_address_history(
    cases = cases,
    locations_clean = locations_clean,
    language_tokens = language_tokens
  )
  cases_vacc_history_clean <- clean_case_vax_history(
    cases = cases,
    language_tokens = language_tokens
  )
  cases_dateranges_history_clean <- clean_case_med_history(
    cases = cases,
    language_tokens = language_tokens
  )

  cases_clean <- clean_cases(
    cases = cases,
    cases_address_history_clean = cases_address_history_clean,
    cases_vacc_history_clean = cases_vacc_history_clean,
    cases_dateranges_history_clean = cases_dateranges_history_clean,
    language_tokens = language_tokens
  )
  contacts_becoming_cases <- cases_from_contacts(cases_clean = cases_clean)

  res <- clean_contacts(
    contacts = contacts,
    contacts_address_history_clean = cases_address_history_clean,
    contacts_vacc_history_clean = cases_vacc_history_clean,
    contacts_becoming_cases = contacts_becoming_cases
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(19L, 48L))
  expect_true(
    all(c(
      "id", "visual_id", "classification", "follow_up_status", "first_name",
      "middle_name", "last_name", "gender", "age", "age_class", "occupation",
      "pregnancy_status", "date_of_reporting", "date_of_last_contact",
      "date_of_burial", "date_of_follow_up_start", "date_of_follow_up_end",
      "was_case", "risk_level", "risk_reason", "safe_burial",
      "transfer_refused", "responsible_user_id", "follow_up_team_id",
      "admin_0_name", "admin_1_name", "admin_2_name", "lat", "long", "address",
      "postal_code", "city", "telephone", "email", "vaccinated", "outcome",
      "date_of_outcome", "relationship_exposure_type",
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
      rep("Date", 5), "logical", "character", "character", "logical", "logical",
      rep("character", 5), "numeric", "numeric", rep("character", 5), "logical",
      "character", "Date", rep("character", 5), "logical", "character",
      "character", "POSIXct", "character", "POSIXct"
    )
  )
})

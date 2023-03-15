test_that("clean_contacts_of_contacts works as expected", {
  skip("get_contacts_of_contacts requires API call")

  contacts_of_contacts <- get_contacts_of_contacts(
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

  contacts_of_contacts_address_history_clean <- clean_contacts_of_contacts_address_history(
    contacts_of_contacts = contacts_of_contacts,
    locations_clean = locations_clean
  )

  contacts_of_contacts_vacc_history_clean <- clean_contacts_of_contacts_vax_history(
    contacts_of_contacts = contacts_of_contacts
  )

  res <- clean_contacts_of_contacts(
    contacts_of_contacts = contacts_of_contacts,
    contacts_of_contacts_address_history_clean = contacts_of_contacts_address_history_clean,
    contacts_of_contacts_vacc_history_clean = contacts_of_contacts_vacc_history_clean
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

test_that("clean_contacts_of_contacts_address_history works as expected", {
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

  language_tokens <- get_language_tokens(
    url = url,
    username = username,
    password = password,
    language = "english_us"
  )

  res <- clean_contacts_of_contacts_address_history(
    contacts_of_contacts = contacts_of_contacts,
    locations_clean = locations_clean,
    language_tokens = language_tokens
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(1L, 13L))
  expect_true(
    all(c(
      "id", "addresses_locationid", "addresses_typeid", "lat", "long",
      "address", "postal_code", "city", "telephone", "email", "admin_0_name",
      "admin_1_name", "admin_2_name"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c(
      "character", "character", "character", "numeric", "numeric",
      rep("logical", 5), "character", "character", "character"
    )
  )
})

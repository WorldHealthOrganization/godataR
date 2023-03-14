test_that("clean_contact_address_history works as expected", {
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

  res <- clean_contact_address_history(
    contacts = contacts,
    locations_clean = locations_clean
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(15L, 13L))
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
      "character", "logical", "character", "character", "character",
      "character", "character", "character"
    )
  )
})

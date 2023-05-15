test_that("clean_case_address_history works as expected", {
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

  language_tokens <- get_language_tokens(
    url = url,
    username = username,
    password = password,
    language = "english_us"
  )

  res <- clean_case_address_history(
    cases = cases,
    locations_clean = locations_clean,
    language_tokens = language_tokens
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(16L, 14L))
  expect_true(
    all(c(
      "id", "visualid", "addresses_locationid", "addresses_typeid", "lat",
      "long", "address", "postal_code", "city", "telephone", "email",
      "admin_0_name", "admin_1_name", "admin_2_name"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c(
      "character", "character", "character", "character", "numeric", "numeric",
      "character", "character", "character", "character", "character",
      "character", "character", "character"
    )
  )
})

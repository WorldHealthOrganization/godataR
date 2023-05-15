test_that("clean_contact_vax_history works as expected", {
  skip("get_contacts requires API call")

  contacts <- get_contacts(
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

  res <- clean_contact_vax_history(
    contacts = contacts,
    language_tokens = language_tokens
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(1L, 5L))
  expect_true(
    all(c(
      "id", "visualid", "vaccinesreceived_vaccine", "vaccinesreceived_date",
      "vaccinesreceived_status"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "Date", "character")
  )
})

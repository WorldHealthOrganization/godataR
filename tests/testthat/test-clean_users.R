test_that("clean_users works as expected", {
  skip("get_users requires API call")

  users <- get_users(url = url, username = username, password = password)

  language_tokens <- get_language_tokens(
    url = url,
    username = username,
    password = password,
    language = "english_us"
  )

  res <- clean_users(users = users, language_tokens = language_tokens)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(20L, 24L))
  expect_true(
    all(c(
      "id", "first_name", "last_name", "email", "institution_name",
      "disregard_geographic_restrictions", paste0("role_ids_", 1:14),
      "active_outbreak_id", "created_by", "datetime_created_at",
      "datetime_last_login"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c(rep("character", 5), "logical", rep("character", 18))
  )
})

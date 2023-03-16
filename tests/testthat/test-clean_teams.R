test_that("clean_teams works as expected", {
  skip("get_teams requires API call")

  teams <- get_teams(url = url, username = username, password = password)
  res <- clean_teams(teams = teams)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(4L, 15L))
  expect_true(
    all(c(
      "id", "name", "user_ids_1", "user_ids_2", "user_ids_3", "user_ids_4",
      "user_ids_5", "location_ids_1", "location_ids_2", "location_ids_3",
      "location_ids_4", "created_by", "datetime_created_at", "updated_by",
      "datetime_updated_at"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    rep("character", 15)
  )
})

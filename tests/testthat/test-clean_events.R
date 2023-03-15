test_that("clean_events works as expected", {
  skip("get_events requires API call")

  res <- get_events(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )
  res <- clean_events(events = res)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(3L, 15L))
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

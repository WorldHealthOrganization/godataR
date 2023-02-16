test_that("clean_case_med_history works as expected", {
  skip("get_cases requires API call")

  res <- get_cases(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )
  res <- clean_case_med_history(cases = res)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(7L, 9L))
  expect_true(
    all(c(
      "id", "visualid", "dateranges_typeid", "dateranges_centername",
      "dateranges_locationid", "dateranges_startdate", "dateranges_enddate",
      "dateranges_comments", "dateranges_id"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c(
      "character", "character", "character", "character", "character", "Date",
      "Date", "logical", "logical"
    )
  )
})

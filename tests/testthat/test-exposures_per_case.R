test_that("exposures_per_case works as expected", {
  skip("get_relationships requires API call")

  relationships <- get_relationships(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )

  clean_relationships <- clean_relationships(relationships)

  res <- exposures_per_case(clean_relationships)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(23L, 2L))
  expect_true(
    all(c(
      "target_person_id", "no_exposures"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "integer")
  )
})

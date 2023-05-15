test_that("clean_relationships works as expected", {
  skip("get_relationships requires API call")

  relationships <- get_relationships(
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

  res <- clean_relationships(
    relationships = relationships,
    language_tokens = language_tokens
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(29L, 11L))
  expect_true(
    all(c(
      "id", "source_person_id", "source_person_visual_id", "target_person_id",
      "target_person_visual_id", "source_person_type", "target_person_type",
      "created_by", "datetime_created_at", "updated_by", "datetime_updated_at"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], function(x) class(x)[1])),
    c(
      "character", "character", "character", "character", "character",
      "character", "character", "character", "POSIXct", "character", "POSIXct"
    )
  )
})

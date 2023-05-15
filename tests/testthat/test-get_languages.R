test_that("get_languages works as expected", {
  skip("get_languages requires API call")

  res <- get_languages(
    url = url,
    username = username,
    password = password
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(6L, 9L))
  expect_identical(
    colnames(res),
    c("name", "readOnly", "id", "createdAt", "createdBy", "updatedAt",
      "updatedBy", "createdOn", "deleted")
  )
  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "logical", "character", "character", "character",
      "character", "character", "character", "logical")
  )
})

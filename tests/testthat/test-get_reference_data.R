test_that("get_reference_data works as expected", {
  skip("get_reference_data requires API call")

  res <- get_reference_data(url = url, username = username, password = password)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(391L, 19L))
  expect_true(
    all(c(
      "categoryId", "value", "description", "readOnly", "active",
      "isDefaultReferenceData", "id", "createdAt", "createdBy", "updatedAt",
      "updatedBy", "createdOn", "deleted", "colorCode", "order",
      "isOutbreakTemplateReferenceData", "deletedAt", "iconId", "code"
    ) %in% colnames(res))
  )


  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "logical", "logical", "logical",
      "character", "character", "character", "character", "character",
      "character", "logical", "character", "integer", "logical", "logical",
      "character", "character"))
})

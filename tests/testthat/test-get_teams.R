test_that("get_teams works as expected", {
  skip("get_teams requires API call")


  res <- get_teams(url = url, username = username, password = password)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(3L, 10L))
  expect_true(
    all(c(
      "name", "userIds", "locationIds", "id", "createdAt", "createdBy",
      "updatedAt", "updatedBy", "createdOn", "deleted"
    ) %in% colnames(res))
  )


  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "list", "list", "character", "character", "character",
      "character", "character", "character", "logical"))
})

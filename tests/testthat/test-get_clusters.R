test_that("get_clusters works as expected", {
  skip("get_clusters requires API call")

  res <- get_clusters(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(1L, 11L))
  expect_identical(
    colnames(res),
    c("name", "description", "icon", "colorCode", "id", "createdAt",
      "createdBy", "updatedAt", "updatedBy", "createdOn", "deleted")
  )
  expect_identical(
    unname(sapply(res[1, ], class)),
    c(rep("character", 10), "logical")
  )
})

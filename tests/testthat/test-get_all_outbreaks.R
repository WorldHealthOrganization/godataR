test_that("get_all_outbreaks works as expected", {
  skip("get_all_outbreaks requires API call")

  res <- get_all_outbreaks(
    url = url,
    username = username,
    password = password
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(11L, 5L))
  expect_identical(
    colnames(res),
    c("id", "name", "description", "createdBy", "createdAt")
  )
  expect_identical(
    unname(sapply(res[1, ], class)),
    rep("character", 5)
  )
})

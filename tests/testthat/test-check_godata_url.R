test_that("check_godata_url works as expected", {
  skip("check_godata_url requires API call")

  res <- check_godata_url(url = url)

  expect_type(res, "logical")
  expect_length(res, 1)
})

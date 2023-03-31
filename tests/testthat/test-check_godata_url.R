test_that("check_godata_url works as expected", {
  skip("check_godata_url requires API call")

  res <- check_godata_url(url = url)

  expect_type(res, "logical")
  expect_length(res, 1)
})

test_that("check_godata_url works as expected specifying success code", {
  skip("check_godata_url requires API call")

  res <- check_godata_url(url = url, success_code = 200)

  expect_type(res, "logical")
  expect_length(res, 1)
})

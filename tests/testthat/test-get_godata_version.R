test_that("get_godata_versions works as expected", {
  skip("get_godata_version requires API call")

  res <- get_godata_version(url = url)

  expect_type(res, "character")
  # character string can contain digits
  expect_true(grepl(pattern = "\\d", x = res))
  # character string can contain full stops
  expect_true(grepl(pattern = ".", x = res))
  # character string cannot contain alphabetic characters
  expect_false(grepl(pattern = "[:alpha:]", x = res))
})

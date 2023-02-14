test_that("get_access_token works as expected", {
  skip("get_access_token requires API call")

  res <- get_access_token(
    url = url,
    username = username,
    password = password
  )

  expect_type(res, "character")
  # character string can contain alphanumeric characters
  expect_true(grepl(pattern = "[:alphanum:]", x = res))
  # character string cannot contain punctuation marks
  expect_false(grepl(pattern = "[[:punct:]]", x = res))
  # character string cannot contain spaces
  expect_false(grepl(pattern = "\\s", x = res))
})

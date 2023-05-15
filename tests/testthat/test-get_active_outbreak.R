test_that("get_active_outbreak works as expected", {
  skip("get_active_outbreak requires API call")

  res <- get_active_outbreak(
    url = url,
    username = username,
    password = password
  )

  expect_type(res, "character")
  # character string can contain alphanumeric characters
  expect_true(grepl(pattern = "[:alphanum:]", x = res))
  # character string can contain punctuation marks
  expect_true(grepl(pattern = "[:punct:]", x = res))
  # character string cannot contain spaces
  expect_false(grepl(pattern = "\\s", x = res))
})

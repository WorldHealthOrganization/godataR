test_that("set_active_outbreak works as expected", {
  skip("set_active_outbreak requires API call")

  expect_message(
    res <- set_active_outbreak(
      url = url,
      username = username,
      password = password,
      outbreak_id = outbreak_id
    ),
    regexp = "Active outbreak not changed."
  )

  expect_type(res, "character")
})

test_that("set_active_outbreak works as expected with non-valid outbreak id", {
  skip("set_active_outbreak requires API call")

  expect_error(
    set_active_outbreak(
      url = url,
      username = username,
      password = password,
      outbreak_id = "123"
    ),
    regexp = paste0(
      "Active outbreak not changed. 123 not in list of user's ",
      "available outbreaks"
    )
  )
})

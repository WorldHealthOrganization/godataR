test_that("get_export_status works as expected", {
  skip("get_export_status requires API call")

  api_call_request <- paste0(
    url, "api/outbreaks/", outbreak_id, "/cases/export"
  )

  export_log_id_request <- httr::GET(
    paste0(
      api_call_request,
      "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22",
      "dontTranslateValues%22%3A%22true%22%2C%20%22",
      "jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
      "&type=",
      "json",
      "&access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )

  export_log_id_request_content <- httr::content(export_log_id_request)

  request_id <- purrr::pluck(export_log_id_request_content, "exportLogId")

  res <- get_export_status(
    url = url,
    username = username,
    password = password,
    request_id = request_id
  )

  expect_type(res, "list")
  expect_length(res, 3)
  expect_named(res, c("statusStep", "totalNo", "processedNo"))
  expect_type(res$statusStep, "character")
  expect_type(res$totalNo, "integer")
  expect_type(res$processedNo, "integer")
})

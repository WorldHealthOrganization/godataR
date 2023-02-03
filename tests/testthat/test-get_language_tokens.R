test_that("get_language_tokens works as expected", {
  skip("get_language_tokens requires API call")

  res <- get_language_tokens(
    url = url,
    username = username,
    password = password,
    language = "english_us"
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(13641L, 3L))
  expect_identical(
    colnames(res),
    c("languageId", "lastUpdateDate", "tokens")
  )
  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "data.frame")
  )
  expect_identical(colnames(res$tokens),c("token", "translation"))
  expect_identical(dim(res$tokens), c(13641L, 2L))
})

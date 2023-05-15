test_that("get_users works as expected", {
  skip("get_users requires API call")

  res <- get_users(url = url, username = username, password = password)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(20L, 67L))
  expect_true(
    all(c(
      "id", "firstName", "lastName", "roleIds", "activeOutbreakId",
      "languageId", "passwordChange", "loginRetriesCount", "lastLoginDate",
      "disregardGeographicRestrictions", "dontCacheFilters", "email",
      "createdAt", "createdBy", "updatedAt", "updatedBy", "deleted",
      "createdOn", "securityQuestions", "outbreakIds", "institutionName"
    ) %in% colnames(res))
  )

  expect_true(
    all(grepl(pattern = "^settings", x = colnames(res)[22:66]))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "list", "character", "character",
      "logical", "integer", "character", "logical", "logical", "character",
      "character", "character", "character", "character", "logical",
      "character", "list", "list", "character", rep("list", 12), "logical",
      rep("list", 29), "logical", "list", "list", "character"))
})

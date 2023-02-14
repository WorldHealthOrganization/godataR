test_that("get_locations works as expected", {
  skip("get_locations requires API call")

  res <- get_locations(url = url, username = username, password = password)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(11L, 16L))
  expect_true(
    all(c(
      "name", "synonyms", "identifiers", "active", "populationDensity",
      "parentLocationId", "geographicalLevelId", "id", "createdAt",
      "createdBy", "updatedAt", "updatedBy", "createdOn", "deleted",
      "geoLocation.lat", "geoLocation.lng"
    ) %in% colnames(res))
  )


  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "list", "list", "logical", "integer", "character",
      "character", "character", "character", "character", "character",
      "character", "character", "logical", "numeric", "numeric"))
})

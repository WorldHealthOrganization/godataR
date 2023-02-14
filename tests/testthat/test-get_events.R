test_that("get_events works as expected", {
  skip("get_events requires API call")

  res <- get_events(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id,
    method = "export",
    batch_size = 50000,
    wait = 2,
    file_type = "json"
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(2L, 34L))
  expect_true(
    all(c(
      "id", "dateOfReporting", "isDateOfReportingApproximate", "createdAt",
      "createdBy", "updatedAt", "updatedBy", "deleted", "deletedAt",
      "createdOn", "type", "numberOfExposures", "numberOfContacts", "name",
      "date", "description", "responsibleUserId", "eventCategory", "endDate",
      "address.typeId", "address.locationId", "address.Identifiers",
      "address.Location geographical level", "address.Parent location",
      "address.geoLocationAccurate", "address.date", "address.country",
      "address.city", "address.addressLine1", "address.postalCode",
      "address.phoneNumber", "address.emailAddress", "address.geoLocation.lat",
      "address.geoLocation.lng"
    ) %in% colnames(res))
  )


  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "logical", "character", "character",
      "character", "character", "logical", "logical", "character", "character",
      "integer", "integer", "character", "character", "logical", "logical",
      "logical", "character", "character", "character", "list", "list",
      "list", "logical", "character", "logical", "logical", "logical",
      "logical", "logical", "logical", "numeric", "numeric"))

})

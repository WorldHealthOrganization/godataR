test_that("get_followups works as expected", {
  skip("get_followups requires API call")

  res <- get_followups(
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
  expect_identical(dim(res), c(49L, 201L))
  expect_true(
    all(c(
      "id", "date", "index", "createdAt", "createdBy", "updatedAt", "updatedBy",
      "deleted", "deletedAt", "createdOn", "fillLocation", "teamId", "statusId",
      "targeted", "comment", "responsibleUserId", "contact.firstName",
      "contact.lastName", "contact.visualId", "contact.id", "address.typeId",
      "address.locationId", "address.Identifiers",
      "address.Location geographical level", "address.Parent location",
      "address.geoLocationAccurate", "address.date", "address.country",
      "address.city", "address.addressLine1", "address.postalCode",
      "address.phoneNumber", "address.emailAddress", "address.geoLocation.lat",
      "address.geoLocation.lng"
    ) %in% colnames(res))
  )

  expect_true(
    all(grepl(pattern = "^questionnaireAnswers", x = colnames(res)[36:201]))
  )


  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "integer", "character", "character",
      "character", "character", "logical", "logical", "character", "logical",
      "character", "character", "logical", "logical", "character", "character",
      "character", "character", "character", "character", "character", "list",
      "list", "list", "logical", "character", "logical", "logical", "logical",
      "logical", "logical", "logical", "numeric", "numeric", "list", "list",
      "list", "list", "list", "list", "list", "logical", "list", "logical",
      "list", "list", rep("logical", 152), "list", "list"))
})

test_that("get_labresults works as expected", {
  skip("get_labresults requires API call")

  res <- get_labresults(
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
  expect_identical(dim(res), c(9L, 55L))
  expect_true(
    all(c(
      "id", "createdAt", "createdBy", "updatedAt", "updatedBy", "deleted",
      "deletedAt", "createdOn", "personId", "dateSampleTaken",
      "dateSampleDelivered", "dateTesting", "dateOfResult", "labName",
      "sampleIdentifier", "sampleType", "testType", "testedFor", "result",
      "quantitativeResult", "notes", "status", "sequence.hasSequence",
      "sequence.dateSampleSent", "sequence.labId", "sequence.dateResult",
      "sequence.resultId", "sequence.noSequenceReason", "person.visualId",
      "person.type", "person.lastName", "person.firstName",
      "person.dateOfOnset", "person.dateOfReporting", "person.middleName",
      "person.address.typeId", "person.address.city",
      "person.address.locationId", "person.address.Identifiers",
      "person.address.Location geographical level",
      "person.address.Parent location", "person.address.geoLocationAccurate",
      "person.address.date", "person.address.country",
      "person.address.addressLine1", "person.address.postalCode",
      "person.address.phoneNumber", "person.address.emailAddress",
      "person.address.geoLocation.lat", "person.address.geoLocation.lng",
      "questionnaireAnswers.Lab_SpecimenCollection_Symptoms",
      "questionnaireAnswers.Lab_SpecimenShippedAnotherLaboratory",
      "questionnaireAnswers.test",
      "questionnaireAnswers.Lab_specimenshipped_laboratoryname",
      "questionnaireAnswers.Lab_specimenshipped_dateofshipping"
    ) %in% colnames(res))
  )


  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "character", "character",
      "logical", "logical", "character", "character", "character", "character",
      "character", "character", "logical", "character", "character",
      "character", "logical", "character", "logical", "logical", "character",
      "logical", "character", "character", "character", "character", "logical",
      "character", "character", "character", "character", "character",
      "character", "character", "character", "character", "character", "list",
      "list", "list", "logical", "character", "logical", "logical", "logical",
      "logical", "logical", "numeric", "numeric", "list", "list", "list",
      "list", "list"))
})

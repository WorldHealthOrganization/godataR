test_that("export_downloader works as expected", {
  skip("export_downloader requires API call")

  api_call_request <- paste0(
    url, "api/outbreaks/", outbreak_id, "/cases/export"
  )
  res <- export_downloader(
    url = url,
    username = username,
    password = password,
    api_call_request = api_call_request,
    wait = 2,
    file.type = "json"
  )

  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(13L, 357L))
  expect_true(
    all(c(
      "id", "visualId", "dateOfReporting", "isDateOfReportingApproximate",
      "createdAt", "createdBy", "updatedAt", "updatedBy", "deleted",
      "deletedAt", "createdOn", "firstName", "middleName", "lastName", "gender",
      "occupation", "dob", "classification", "wasContact", "dateBecomeCase",
      "wasCase", "dateOfInfection", "dateOfOnset", "riskLevel", "riskReason",
      "outcomeId", "dateOfOutcome", "documents", "type", "dateRanges",
      "transferRefused", "addresses", "safeBurial", "dateOfBurial",
      "isDateOfOnsetApproximate", "numberOfExposures", "numberOfContacts",
      "burialLocationId", "burialLocationId Identifiers",
      "burialLocationId Location geographical level",
      "burialLocationId Parent location", "burialPlaceName",
      "investigationStatus", "dateInvestigationCompleted", "vaccinesReceived",
      "pregnancyStatus", "responsibleUserId", "age.years", "age.months"
    ) %in% colnames(res))
  )

  expect_true(
    all(grepl(pattern = "^questionnaireAnswers", x = colnames(res)[50:357]))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "logical", "character",
      "character", "character", "character", "logical", "logical", "character",
      "character", "character", "character", "character", "character",
      "character", "character", "logical", "character", "logical", "character",
      "character", "character", "character", "character", "character", "list",
      "character", "list", "logical", "list", "logical", "logical", "logical",
      "integer", "integer", "logical", "list", "list", "list", "logical",
      "character", "logical", "list", "character", "character", "integer",
      "integer", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "list", "list",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "list", "list", "list",
      "list", "list", "list", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "list", "list", "list",
      "list", "list", "list", "list", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "logical",
      "list", "list", "list", "list", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "list", "logical", "logical", "list", "logical", "logical",
      "logical", "list", "logical", "list", "logical", "list", "logical",
      "list", "logical", "list", "logical", "list", "logical", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "logical", "list", "logical", "list",
      "logical", "logical", "logical", "logical", "list", "logical", "logical",
      "logical", "logical", "list", "logical", "list", "logical", "list",
      "list", "logical", "list", "logical", "list", "logical", "list",
      "logical", "list", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "list", "list", "list"))
})

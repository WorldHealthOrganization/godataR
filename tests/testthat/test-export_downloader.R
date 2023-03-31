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
    file_type = "json"
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(14L, 363L))
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
      "deathLocationId", "deathLocationId Identifiers",
      "deathLocationId Location geographical level",
      "deathLocationId Parent location", "burialLocationId",
      "burialLocationId Identifiers",
      "burialLocationId Location geographical level",
      "burialLocationId Parent location", "burialPlaceName",
      "investigationStatus", "dateInvestigationCompleted", "vaccinesReceived",
      "pregnancyStatus", "age.years", "age.months", "responsibleUser.firstName",
      "responsibleUser.lastName", "responsibleUser.id"
    ) %in% colnames(res))
  )

  expect_true(
    all(grepl(pattern = "^questionnaireAnswers", x = colnames(res)[56:363]))
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
      "list", "list", "list", "logical", "character", "logical", "list",
      "character", "integer", "integer", "character", "character", "character",
      "list", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "list", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "list", "list", "list", "list", "list",
      "list", "logical", "logical", "logical", "logical", "logical", "logical",
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
      "logical", "logical", "list", "list", "list", "list", "list", "list",
      "list", "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "logical", "list", "list", "list", "list", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "logical", "list", "logical", "logical", "list",
      "logical", "logical", "logical", "list", "logical", "list", "logical",
      "list", "logical", "list", "logical", "list", "logical", "list",
      "logical", "list", "list", "list", "list", "list", "list", "list", "list",
      "list", "list", "list", "list", "list", "list", "list", "logical", "list",
      "logical", "list", "logical", "logical", "logical", "logical", "list",
      "logical", "logical", "logical", "logical", "list", "logical", "list",
      "logical", "list", "list", "logical", "list", "logical", "list",
      "logical", "list", "logical", "list", "logical", "logical", "logical",
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
      "logical", "logical", "logical", "logical", "logical", "list", "list",
      "list")
  )
})

test_that("get_contacts_of_contacts works as expected", {
  skip("get_contacts_of_contacts requires API call")

  res <- get_contacts_of_contacts(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(1L, 39L))
  expect_identical(
    colnames(res),
    c("id", "visualId", "dateOfReporting", "isDateOfReportingApproximate",
      "createdAt", "createdBy", "updatedAt", "updatedBy", "deleted",
      "deletedAt", "createdOn", "firstName", "middleName", "lastName",
      "gender", "occupation", "dob", "classification", "wasContact", "wasCase",
      "dateBecomeContact", "riskLevel", "riskReason", "outcomeId",
      "dateOfOutcome", "documents", "type", "transferRefused", "addresses",
      "safeBurial", "dateOfBurial", "dateOfLastContact", "numberOfExposures",
      "vaccinesReceived", "pregnancyStatus", "responsibleUserId",
      "relationship", "age.years", "age.months")
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "logical", "character",
      "character", "character", "character", "logical", "logical", "character",
      "character", "logical", "character", "character", "logical", "logical",
      "logical", "logical", "logical", "logical", "logical", "logical",
      "logical", "logical", "list", "character", "logical", "list", "logical",
      "logical", "character", "integer", "list", "character", "logical",
      "logical", "integer", "logical")
  )
})

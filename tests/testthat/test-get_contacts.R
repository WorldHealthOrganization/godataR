test_that("get_contacts works as expected", {
  skip("get_contacts requires API call")

  res <- get_contacts(
    url = url,
    username = username,
    password = password,
    outbreak_id = outbreak_id
  )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(14L, 64L))
  expect_identical(
    colnames(res),
    c("id", "visualId", "dateOfReporting", "isDateOfReportingApproximate",
      "createdAt", "createdBy", "updatedAt", "updatedBy", "deleted",
      "deletedAt", "createdOn", "firstName", "middleName", "lastName", "gender",
      "occupation", "dob", "classification", "wasContact", "wasCase",
      "dateBecomeContact", "riskLevel", "riskReason", "outcomeId",
      "dateOfOutcome", "documents", "type", "transferRefused", "addresses",
      "safeBurial", "dateOfBurial", "followUpTeamId", "dateOfLastContact",
      "numberOfExposures", "numberOfContacts", "vaccinesReceived",
      "pregnancyStatus", "responsibleUserId", "age.years", "age.months",
      "followUp.originalStartDate", "followUp.startDate", "followUp.endDate",
      "followUp.status", "relationship.contactDate",
      "relationship.contactDateEstimated", "relationship.certaintyLevelId",
      "relationship.createdAt", "relationship.createdBy",
      "relationship.updatedAt", "relationship.updatedBy",
      "relationship.createdOn", "relationship.deleted",
      "relationship.relatedId", "relationship.id",
      "relationship.exposureTypeId", "relationship.exposureFrequencyId",
      "relationship.exposureDurationId",
      "relationship.socialRelationshipTypeId",
      "relationship.socialRelationshipDetail", "relationship.clusterId",
      "relationship.comment", "relationship.deletedAt",
      "questionnaireAnswers.test")
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "logical", "character",
      "character", "character", "character", "logical", "logical", "character",
      "character", "logical", "character", "character", "character", "logical",
      "character", "logical", "logical", "character", "character", "character",
      "logical", "logical", "list", "character", "logical", "list", "logical",
      "logical", "character", "character", "integer", "integer", "list",
      "logical", "character", "integer", "integer", "character", "character",
      "character", "character", "character", "logical", "character",
      "character", "character", "character", "character", "character",
      "logical", "character", "character", "character", "character",
      "character", "character", "character", "logical", "logical", "logical",
      "list")
  )
})

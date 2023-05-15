test_that("get_relationships works as expected", {
  skip("get_relationships requires API call")

  res <- get_relationships(
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
  expect_identical(dim(res), c(29L, 43L))
  expect_true(
    all(c(
      "id", "createdAt", "createdBy", "updatedAt", "updatedBy", "deleted",
      "deletedAt", "createdOn", "dateOfFirstContact", "contactDate",
      "contactDateEstimated", "certaintyLevelId", "exposureTypeId",
      "exposureFrequencyId", "exposureDurationId", "socialRelationshipTypeId",
      "socialRelationshipDetail", "clusterId", "comment", "sourcePerson.type",
      "sourcePerson.firstName", "sourcePerson.gender", "sourcePerson.visualId",
      "sourcePerson.lastName", "sourcePerson.dob", "sourcePerson.id",
      "sourcePerson.source", "sourcePerson.name", "sourcePerson.middleName",
      "sourcePerson.age.years", "sourcePerson.age.months", "targetPerson.type",
      "targetPerson.firstName", "targetPerson.gender", "targetPerson.visualId",
      "targetPerson.lastName", "targetPerson.id", "targetPerson.target",
      "targetPerson.name", "targetPerson.middleName", "targetPerson.dob",
      "targetPerson.age.years", "targetPerson.age.months"
    ) %in% colnames(res))
  )


  expect_identical(
    unname(sapply(res[1, ], class)),
    c("character", "character", "character", "character", "character",
      "logical", "logical", "character", "character", "character", "logical",
      "character", "character", "character", "character", "character",
      "character", "character", "character", "character", "character",
      "character", "character", "character", "logical", "character", "logical",
      "logical", "character", "integer", "integer", "character", "character",
      "character", "character", "character", "character", "logical",
      "character", "character", "character", "integer", "integer"))
})

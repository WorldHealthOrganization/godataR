test_that("clean_locations works as expected", {
  skip("get_locations requires API call")

  res <- get_locations(
    url = url,
    username = username,
    password = password
  )
  res <- clean_locations(locations = res)

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(11L, 24L))
  expect_true(
    all(c(
      "location_id", "admin_level", "name", "parent_location_id", "lat",
      "long", "admin_0_location_id", "admin_0_admin_level", "admin_0_name",
      "admin_0_parent_location_id", "admin_0_lat", "admin_0_long",
      "admin_1_location_id", "admin_1_admin_level", "admin_1_name",
      "admin_1_parent_location_id", "admin_1_lat", "admin_1_long",
      "admin_2_location_id", "admin_2_admin_level", "admin_2_name",
      "admin_2_parent_location_id", "admin_2_lat", "admin_2_long"
    ) %in% colnames(res))
  )

  expect_identical(
    unname(sapply(res[1, ], class)),
    c(
      "character", "character", "character", "character", "numeric", "numeric",
      "character", "character", "character", "character", "numeric", "numeric",
      "character", "character", "character", "character", "numeric", "numeric",
      "character", "character", "character", "character", "numeric", "numeric"
    )
  )
})

#' Cleans location data
#'
#' @description Rearrange via joins to get into more usable hierarchy format,
#' these can then be joined to cases, contacts, etc for further analysis
#'
#' @param locations A [`tibble`] containing locations data. This is the data
#' returned from [`get_locations()`]
#'
#' @return A `tibble` containing the cleaned and rearranged location data.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' locations <- get_locations(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#'
#' clean_locations(locations = locations)
#' }
clean_locations <- function(locations) {

  # filter out delete and inactive (or NA) values
  clean_locations <- dplyr::filter(
    locations,
    .data$deleted == FALSE | is.na(.data$deleted)
  )
  clean_locations <- dplyr::filter(
    clean_locations,
    .data$active == TRUE | is.na(.data$active)
  )

  # add admin-level column
  clean_locations <- dplyr::mutate(
    .data = clean_locations,
    admin_level = sub(".*LEVEL_", "", .data$geographicalLevelId)
  )

  # select columns
  clean_locations <- dplyr::select(
    .data = clean_locations,
    location_id = "id",
    "admin_level",
    "name",
    parent_location_id = "parentLocationId",
    lat = "geoLocation.lat",
    long = "geoLocation.lng"
  )

  clean_locations <- dplyr::filter(
    .data = clean_locations,
    !is.na(.data$admin_level)
  )

  # split locations data frames into separate data frames by admin level
  locations_split <- dplyr::group_by(
    .data = clean_locations,
    .data$admin_level
  )
  locations_split <- dplyr::group_split(locations_split)

  # rename columns by appending admin level
  locations_split <- purrr::imap(.x = locations_split, .f = function(x, idx) {
    colnames(x) <- paste("admin", idx - 1, colnames(x), sep = "_")
    x
  })

  # add location_id and parent_location_id columns
  locations_split <- purrr::imap(.x = locations_split, .f = function(x, idx) {
    x$location_id <- dplyr::pull(
      x, paste("admin", idx - 1, "location_id", sep = "_")
    )
    x
  })
  locations_split <- purrr::imap(.x = locations_split, .f = function(x, idx) {
    if (idx - 1 != 0) {
      x$parent_location_id <- dplyr::pull(
        x, paste("admin", idx - 1, "parent_location_id", sep = "_")
      )
    }
    x
  })

  # loop over list of admin specific data frames and join them
  for (i in seq(from = length(locations_split), to = 2L)) {

    for (x in 1:(i - 1)) {

      join_index <- i - x

      locations_split[[i]] <- dplyr::left_join(
        x = locations_split[[i]],
        y = locations_split[[join_index]],
        by = c("parent_location_id" = "location_id")
      )

      # first table (admin level 0) does not contain parent_location_id so skip
      if (join_index != 1) {
        # use parent_location_id from right table
        locations_split[[i]]$parent_location_id <-
          locations_split[[i]]$parent_location_id.y
        # remove extra parent_location_id column
        locations_split[[i]]$parent_location_id.y <- NULL
      }
    }
    locations_split[[i]]$parent_location_id <- NULL
  }

  # bind the admin level tables by row
  full <- do.call(dplyr::bind_rows, locations_split)

  # join cleaned location with new table
  clean_locations <- left_join(
    x = clean_locations,
    y = full,
    by = "location_id"
  )

  return(clean_locations)
}

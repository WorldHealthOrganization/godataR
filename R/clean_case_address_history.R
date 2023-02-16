#' Extract address information from case data
#'
#' @description This function un-nests and cleans the address data and stores
#' it in a standalone table with all addresses, even if there is more than 1
#' per person.
#'
#' @param cases A tibble with case data. Case data is returned by
#' [`get_cases()`].
#'
#' @return A tibble with address information from cases data.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' cases <- get_cases(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#'
#' case_address_history <- clean_case_address_history(cases = cases)
#' }
clean_case_address_history <- function(cases) {

  cases_address_history_clean <- dplyr::filter(
    .data = cases,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  cases_address_history_clean <- dplyr::select(
    .data = cases_address_history_clean,
    "id", "visualId", "addresses"
  )

  cases_address_history_clean <- tidyr::unnest(
    data = cases_address_history_clean,
    cols = "addresses",
    names_sep = "_")

  cases_address_history_clean <- dplyr::select_all(
    .tbl = cases_address_history_clean,
    .funs = ~gsub("\\.", "_", tolower(.))
  )

  cases_address_history_clean <- dplyr::select_if(
    .tbl = cases_address_history_clean,
    purrr::negate(is.list)
  )

  cases_address_history_clean <- dplyr::mutate(
    .data = cases_address_history_clean,
    addresses_typeid = sub(".*TYPE_", "", .data$addresses_typeid)
  )

  cases_address_history_clean <- dplyr::left_join(
    cases_address_history_clean,
    locations_clean,
    by = c("addresses_locationid" = "location_id")
  )

  # bring in GPS from locations in case blank from case record, otherwise use
  # case
  cases_address_history_clean <- dplyr::mutate(
    .data = cases_address_history_clean,
    lat = dplyr::case_when(
      is.na(addresses_geolocation_lat) ~ lat,
      TRUE ~ addresses_geolocation_lat
    ),
    long = dplyr::case_when(
      is.na(addresses_geolocation_lng) ~ lat,
      TRUE ~ addresses_geolocation_lng
    )
  )

  cases_address_history_clean <- dplyr::select(
    .data = cases_address_history_clean,
    "id",
    "visualid",
    "addresses_locationid",
    "addresses_typeid",
    "lat",
    "long",
    address = "addresses_addressline1",
    postal_code = "addresses_postalcode",
    city = "addresses_city",
    telephone = "addresses_phonenumber",
    email = "addresses_emailaddress",
    dplyr::matches("^admin_.*name$")
  )

  return(cases_address_history_clean)
}

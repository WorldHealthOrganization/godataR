#' Clean events data
#'
#' @description Cleans and un-nests events data which is returned from
#' [`get_events()`].
#'
#' @param events A `tibble` with events data. Events data is returned by
#' [`get_events()`].
#' @param locations_clean A `tibble` with cleaned location data. Location data
#' is returned by [`get_locations()`] and cleaned by [`clean_locations()`].
#' Make sure the locations data is cleaned prior to supplying it to
#' `clean_events()`.
#'
#' @return A `tibble` with cleaned events data.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' events <- get_events(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#'
#' locations <- get_locations(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#' locations_clean <- clean_locations(locations = locations)
#'
#' clean_events <- clean_events(
#'   events = events,
#'   locations_clean = locations_clean)
#' }
clean_events <- function(events,
                         locations_clean) {

  # Remove all deleted records
  clean_events <- dplyr::filter(
    .data = events,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # Remove all nested fields, otherwise problems with exporting to excel
  clean_events <- dplyr::select_if(.tbl = clean_events, purrr::negate(is.list))

  # standardize column name syntax
  clean_events <- janitor::clean_names(clean_events)

  # label timestamps as datetime
  clean_events <- dplyr::rename(
    .data = clean_events,
    datetime_updated_at = "updated_at",
    datetime_created_at = "created_at"
  )

  # clean up all character fields
  clean_events <- dplyr::mutate(
    .data = clean_events,
    dplyr::across(dplyr::where(is.character), na_if, "")
  )

  # clean date formats (TODO: edit this so that we can see time stamps)
  clean_events <- mutate_at(
    clean_events,
    dplyr::vars(dplyr::starts_with("date_")),
    list(~ as.Date(substr(., 1, 10)))
  )
  clean_events <- mutate(
    clean_events,
    datetime_updated_at = as.POSIXct(datetime_updated_at, format="%Y-%m-%dT%H:%M"))
  clean_events <- mutate(
    clean_events,
    datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M"))

  clean_events <- dplyr::left_join(
    x = clean_events,
    y = select(locations_clean,
               location_id,
               matches("^admin_.*name$")),
    by = c("address_location_id" = "location_id")
  )

  # organize order of vars, only bring in what we need, take away
  # confusing vars
  clean_events <- dplyr::select(
    .data = clean_events,
    "id", # identifier
    "name", # identifier
    "date", # dates
    "date_of_reporting", # dates
    "description",
    "responsible_user", # assigned contact tracer
    matches("^admin_.*name$"),
    lat = "address_geo_location_lat", # address
    long = "address_geo_location_lng", # address
    address = "address_address_line1", # address
    postal_code = "address_postal_code", # address
    city = "address_city", # address
    telephone = "address_phone_number", # address
    email = "address_email_address", # address
    location_id = "address_location_id",  # uuid in case need later for joining of whatever sort.
    "created_by",
    "datetime_created_at",
    "updated_by",
    "datetime_updated_at"
  )  # record modification

  return(clean_events)
}

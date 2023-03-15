#' Clean followup data
#'
#' @description Cleans and un-nests followup data which is returned from
#' [`get_followups()`]
#'
#' @param followups A `tibble` with events data. Followup data is returned by
#' [`get_followups()`].
#' @param contacts_address_history_clean A `tibble` with cleaned address
#' history data from contacts. Contacts data is returned by [`get_contacts()`]
#' and cleaned by [`clean_contact_address_history()`].
#'
#' @return A `tibble` with cleaned followup data.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' followups <- get_followups(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#'
#' contacts <- get_contacts(
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
#'
#' locations_clean <- clean_locations(locations = locations)
#'
#' contacts_address_history_clean <- clean_contact_address_history(
#'   contacts = contacts,
#'   locations_clean = locations_clean
#' )
#'
#' followups_clean <- clean_followups(
#'   followups = followups,
#'   contacts_address_history_clean = contacts_address_history_clean
#' )
#' }
clean_followups <- function(followups,
                            contacts_address_history_clean) {

  # Remove all deleted records
  followups_clean <- dplyr::filter(
    .data = followups,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # Remove all nested fields, otherwise problems with exporting to excel
  followups_clean <- dplyr::select_if(
    .tbl = followups_clean,
    .predicate = purrr::negate(is.list)
  )

  # take out all that are not core variables, otherwise diff versions and
  # problems exporting to excel
  followups_clean <- dplyr::select(
    .data = followups_clean,
    -dplyr::contains("questionnaireAnswers")
  )

  # standardize column name syntax
  followups_clean <- janitor::clean_names(dat = followups_clean)

  # label timestamps as datetime
  followups_clean <- dplyr::rename(
    .data = followups_clean,
    datetime_updated_at = "updated_at",
    datetime_created_at = "created_at"
  )

  # clean up all character fields
  followups_clean <- dplyr::mutate(
    .data = followups_clean,
    dplyr::across(dplyr::where(is.character), dplyr::na_if, "")
  )

  # clean date formats (TODO: edit this so that we can see time stamps)
  followups_clean <- dplyr::mutate_at(
    .tbl = followups_clean,
    dplyr::vars(date), list(~ as.Date(substr(., 1, 10)))
  )

  followups_clean <- dplyr::mutate(
    .data = followups_clean,
    datetime_updated_at = as.POSIXct(datetime_updated_at, format = "%Y-%m-%dT%H:%M")
  )

  followups_clean <- dplyr::mutate(
    .data = followups_clean,
    datetime_created_at = as.POSIXct(datetime_created_at, format = "%Y-%m-%dT%H:%M")
  )

  #  truncate responses of categorical vars so easier to read
  followups_clean <- dplyr::mutate(
    .data = followups_clean,
    followup_status = sub(".*TYPE_", "", status_id)
  )

  contacts_address_history_clean <- dplyr::filter(
    .data = contacts_address_history_clean,
    addresses_typeid == "USUAL_PLACE_OF_RESIDENCE"
  )

  followups_clean <- dplyr::left_join(
    x = followups_clean,
    y = contacts_address_history_clean,
    by = "id"
  )

  # organize order of vars, only bring in what we need, take away confusing vars
  followups_clean <- dplyr::select(
    .data = followups_clean,
    "id", # identifier
    "contact_id", # identifier
    "contact_visual_id", # identifier
    "date", # dates
    followup_number = "index", # FU status
    "followup_status", # FU status
    "targeted", # FU status
    "responsible_user_id", # assigned contact tracer
    "team_id", # assigned contact tracer
    dplyr::matches("^admin_.*name$"), # address
    "lat", # address
    "long", # address
    "address", # address
    "postal_code", # address
    "city", # address
    "telephone", # address
    "email", # address
    location_id = "addresses_locationid",  # uuid in case need later for joining of whatever sort.
    "created_by",  # record modification
    "datetime_created_at",   # record modification
    "updated_by",   # record modification
    "datetime_updated_at"  # record modification
  )

  return(followups_clean)
}

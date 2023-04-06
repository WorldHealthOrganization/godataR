#' Extracts address information from contact data
#'
#' @description This function un-nests and cleans the address data and stores
#' it in a standalone table with all addresses, even if there is more than 1
#' per person.
#'
#' @param contacts A tibble with contacts data. Contacts data is returned by
#' [`get_contacts()`].
#' @param locations_clean A tibble with cleaned locations data. Locations data
#' is returned by [`get_locations()`] and cleaned by [`clean_locations()`].
#' @param language_tokens A tibble of language tokens returned by
#' [`get_language_tokens()`] to translate the string tokens in the data.
#'
#' @return A tibble with address information from contacts data.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
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
#' locations_clean <- clean_locations(locations = locations)
#'
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = "english_us"
#' )
#'
#' contact_address_history <- clean_contact_address_history(
#'   contacts = contacts,
#'   locations_clean = locations_clean,
#'   language_tokens = language_tokens
#' )
#' }
clean_contact_address_history <- function(contacts,
                                          locations_clean,
                                          language_tokens) {

  contacts_address_history_clean <- dplyr::filter(
    .data = contacts,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  contacts_address_history_clean <- dplyr::select(
    .data = contacts_address_history_clean,
    "id", "visualId", "addresses"
  )

  contacts_address_history_clean <- tidyr::unnest(
    data = contacts_address_history_clean,
    cols = "addresses",
    names_sep = "_"
  )

  contacts_address_history_clean <- dplyr::select_all(
    .tbl = contacts_address_history_clean,
    .funs = ~gsub("\\.", "_", tolower(.))
  )

  contacts_address_history_clean <- dplyr::select_if(
    .tbl = contacts_address_history_clean,
    purrr::negate(is.list)
  )

  contacts_address_history_clean <- translate_categories(
    data = contacts_address_history_clean,
    language_tokens = language_tokens
  )

  contacts_address_history_clean <- dplyr::left_join(
    x = contacts_address_history_clean,
    y = locations_clean,
    by = c("addresses_locationid" = "location_id")
  )

  # bring in GPS from locations if blank in contact record, otherwise use
  # contact address block
  contacts_address_history_clean <- dplyr::mutate(
    .data = contacts_address_history_clean,
    lat = dplyr::case_when(
      is.na(addresses_geolocation_lat) ~ lat, TRUE ~ addresses_geolocation_lat),
    long = dplyr::case_when(
      is.na(addresses_geolocation_lng) ~ lat, TRUE ~ addresses_geolocation_lng)
    )

  contacts_address_history_clean <- dplyr::select(
    .data = contacts_address_history_clean,
    "id",
    "addresses_locationid",
    "addresses_typeid",
    "lat",
    "long",
    "address" = addresses_addressline1,
    "postal_code" = addresses_postalcode,
    "city" = addresses_city,
    "telephone" = addresses_phonenumber,
    "email" = addresses_emailaddress,
    dplyr::matches("^admin_.*name$")
  )

  return(contacts_address_history_clean)
}

#' Extracts address information from contacts of contacts data
#'
#' @description This function un-nests and cleans the address data and stores
#' it in a standalone table with all addresses, even if there is more than 1
#' per person.
#'
#' @param contacts_of_contacts A`tibble` with contacts of contacts data.
#' Contacts of contacts data is returned by [`get_contacts_of_contacts()`].
#' @param locations_clean A `tibble` with cleaned location data. Location data
#' is returned by [`get_locations()`] and cleaned by [`clean_locations()`].
#' @param language_tokens A tibble of language tokens returned by
#' [`get_language_tokens()`] to translate the string tokens in the data.
#'
#' @return A `tibble` with address information from contacts of contacts data.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' contacts_of_contacts <- get_contacts_of_contacts(
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
#' contact_of_contacts_add_hist <- clean_contacts_of_contacts_address_history(
#'   contacts_of_contacts = contacts_of_contacts,
#'   locations_clean = locations_clean,
#'   language_tokens = language_tokens
#' )
#' }
clean_contacts_of_contacts_address_history <- function(contacts_of_contacts,
                                                       locations_clean,
                                                       language_tokens) {

  coc_add_hist <- dplyr::filter(
    .data = contacts_of_contacts,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  coc_add_hist <- dplyr::select(
    .data = coc_add_hist,
    "id", "visualId", "addresses"
  )

  coc_add_hist <- tidyr::unnest(
    data = coc_add_hist,
    "addresses",
    names_sep = "_"
  )

  coc_add_hist <- dplyr::select_all(
    .tbl = coc_add_hist,
    .funs = ~gsub("\\.", "_", tolower(.))
  )

  coc_add_hist <- dplyr::select_if(
    .tbl = coc_add_hist,
    .predicate = purrr::negate(is.list)
  )

  coc_add_hist <- translate_categories(
    data = coc_add_hist,
    language_tokens = language_tokens
  )

  coc_add_hist <- dplyr::left_join(
    x = coc_add_hist,
    y = locations_clean,
    by = c("addresses_locationid" = "location_id")
  )

  # bring in GPS from locations if blank in contact record, otherwise use
  # contact address block
  coc_add_hist <- dplyr::mutate(
    .data = coc_add_hist,
    lat = case_when(
      is.na(addresses_geolocation_lat) ~ lat, TRUE ~ addresses_geolocation_lat),
    long = case_when(
      is.na(addresses_geolocation_lng) ~ lat, TRUE ~ addresses_geolocation_lng)
  )

  coc_add_hist <- dplyr::select(
    .data = coc_add_hist,
    "id",
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

  return(coc_add_hist)
}

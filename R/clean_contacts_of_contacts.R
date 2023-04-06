#' Clean contacts of contacts data
#'
#' @description Cleans and un-nests contacts of contacts data. Contacts of
#' contacts data is returned by [`get_contacts_of_contacts()`].
#'
#' @param contacts_of_contacts A `tibble` containing the contacts of contacts
#' data.
#' @param contacts_of_contacts_address_history_clean A `tibble` containing the
#' cleaned address history data from contacts of contacts (data is cleaned by
#' [`clean_contacts_of_contacts_address_history()`]).
#' @param contacts_of_contacts_vacc_history_clean A `tibble` containing the
#' cleaned vaccination history from contacts of contacts (data is cleaned by
#' [`clean_contacts_of_contacts_vax_history()`]).
#' @param language_tokens A tibble of language tokens returned by
#' [`get_language_tokens()`] to translate the string tokens in the data.
#'
#' @return A `tibble` containing the cleaned contacts of contacts data.
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
#'
#' locations_clean <- clean_locations(locations = locations)
#'
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = "english_us"
#' )
#'
#' contacts_of_contacts_address_history_clean <- clean_contacts_of_contacts_address_history(
#'   contacts_of_contacts = contacts_of_contacts,
#'   locations_clean = locations_clean,
#'   language_tokens = language_tokens
#' )
#'
#' contacts_of_contacts_vacc_history_clean <- clean_contacts_of_contacts_vax_history(
#'   contacts_of_contacts = contacts_of_contacts,
#'   language_tokens = language_tokens
#' )
#'
#' contacts_of_contacts_clean <- clean_contacts_of_contacts(
#'   contacts_of_contacts = contacts_of_contacts,
#'   contacts_of_contacts_address_history_clean = contacts_of_contacts_address_history_clean,
#'   contacts_of_contacts_vacc_history_clean = contacts_of_contacts_vacc_history_clean,
#'   language_tokens = language_tokens
#' )
#' }
clean_contacts_of_contacts <- function(contacts_of_contacts,
                                       contacts_of_contacts_address_history_clean,
                                       contacts_of_contacts_vacc_history_clean,
                                       language_tokens) {

  # Remove all deleted records
  coc_clean <- dplyr::filter(
    .data = contacts_of_contacts,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # Remove all nested fields, otherwise problems with exporting to excel
  coc_clean <- dplyr::select_if(
    .tbl = coc_clean,
    .predicate = purrr::negate(is.list)
  )

  # standardize column name syntax
  coc_clean <- janitor::clean_names(dat = coc_clean)

  # label timestamps as datetime
  coc_clean <- dplyr::rename(
    .data = coc_clean,
    date_of_birth = "dob",
    datetime_updated_at = "updated_at",
    datetime_created_at = "created_at"
  )

  # clean up all character fields
  coc_clean <- dplyr::mutate(
    .data = coc_clean,
    dplyr::across(dplyr::where(is.character), na_if, "")
  )

  # clean date formats (TODO: edit this so that we can see time stamps)
  coc_clean <- dplyr::mutate_at(
    .tbl = coc_clean,
    .vars = dplyr::vars(dplyr::starts_with("date_")),
    list(~ as.Date(substr(., 1, 10)))
  )

  coc_clean <- dplyr::mutate(
    .data = coc_clean,
    datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")
  )

  coc_clean <- dplyr::mutate(
    .data = coc_clean,
    datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")
  )

  #  truncate responses of categorical vars so easier to read
  coc_clean <- translate_categories(
    data = coc_clean,
    language_tokens = language_tokens
  )

  contacts_of_contacts_address_history_clean <- dplyr::filter(
    .data = contacts_of_contacts_address_history_clean,
    addresses_typeid == "Current address"
  )

  # join in current address from address history, only current place of
  # residence
  coc_clean <- left_join(
    x = coc_clean,
    y = contacts_of_contacts_address_history_clean,
    by="id"
  )

  # join in info from vacc block
  coc_clean <- dplyr::mutate(
    .data = coc_clean,
    vaccinated = case_when(id %in% contacts_of_contacts_vacc_history_clean$id[contacts_of_contacts_vacc_history_clean$vaccinesreceived_status == "VACCINATED"] ~ TRUE, TRUE ~ FALSE)
  )

  # force NA ages to appear as NA, not as 0 like sometimes occurs
  coc_clean <- dplyr::mutate(.data = coc_clean, age_years = as.numeric(age_years))
  coc_clean <- dplyr::mutate(.data = coc_clean, age_years = na_if(age_years,0))
  coc_clean <- dplyr::mutate(.data = coc_clean, age_months = as.numeric(age_months))
  coc_clean <- dplyr::mutate(.data = coc_clean, age_months = na_if(age_months,0))

  # standardize age vars into just one var, round by 1 decimal
  coc_clean <- dplyr::mutate(
    .data = coc_clean,
    age = case_when(!is.na(age_months) ~  round(age_months / 12, digits = 1),
                    TRUE ~ age_years)
  )

  # WHO age categories updated Sept 2020:
  # 0-4, 5-9, 10-14, 15-19, 20-29, 30-39, 40-49, 50-59, 60-64, 65-69, 70-74,
  # 75-79, 80+
  # these categories below match that of detailed WHO surveillance dash:
  # <5, 5-14, 15-24, 25-64, 65+
  coc_clean <- dplyr::mutate(
    .data = coc_clean,
    age_class = factor(
      case_when(
        age <= 4 ~ "<5",
        age <= 14 ~ "5-14",
        age <= 24 ~ "15-24",
        age <= 64 ~ "25-64",
        is.finite(age) ~ "65+",
        TRUE ~ "unknown"
      ), levels = c(
        "<5",
        "5-14",
        "15-24",
        "25-64",
        "65+",
        "unknown"
      )),
    age_class = factor(
      age_class,
      levels = rev(levels(age_class)))
  )

  # organize order of vars, only bring in what we need, take away confusing vars
  coc_clean <- dplyr::select(
    .data = coc_clean,
    "id", # identifier
    "visual_id", # identifier
    "classification", # identifier
    "first_name", # demographics
    "middle_name", # demographics
    "last_name", # demographics
    "gender", # demographics
    "age", # demographics
    "age_class", # demographics
    "occupation", # demographics
    "pregnancy_status", # demographics
    "date_of_reporting", # dates
    "date_of_last_contact", # dates
    "date_of_burial", # dates
    "was_case", # epi
    "risk_level", # epi
    "risk_reason", # epi
    "safe_burial", # epi
    "transfer_refused", # epi
    "responsible_user_id", # assigned contact tracer
    dplyr::matches("^admin_.*name$"), # address
    "lat",# address
    "long", # address
    "address", # address
    "postal_code", # address
    "city", # address
    "telephone", # address
    "email", # address
    "vaccinated", # vaccination
    "outcome", # outcome
    "date_of_outcome",  # outcome
    "relationship_exposure_type",
    "relationship_context_of_transmission",
    "relationship_exposure_duration",
    "relationship_exposure_frequency",
    "relationship_certainty_level",
    "relationship_cluster_id",
    location_id = "addresses_locationid",  # uuid in case need later for joining of whatever sort.
    "created_by",
    "datetime_created_at",
    "updated_by",
    "datetime_updated_at"
  )

  return(coc_clean)
}

#' Pull out all cases that used to be contacts
#'
#' @param cases_clean The cleaned case data. Case data is returned by
#' [`get_cases()`] and cleaned by [`clean_cases()`].
#'
#' @return A tibble containing the cases that used to be contacts.
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
#' # other cleaned data required for `clean_cases()`
#' cases_vacc_history_clean <- clean_case_vax_history(cases = cases)
#' cases_address_history_clean <- clean_case_address_history(cases = cases)
#' cases_dateranges_history_clean <- clean_case_med_history(cases = cases)
#'
#' cases_clean <- clean_cases(
#'   cases = cases,
#'   cases_address_history_clean = cases_address_history_clean,
#'   cases_vacc_history_clean = cases_vacc_history_clean,
#'   cases_dateranges_history_clean = cases_dateranges_history_clean
#' )
#'
#' cases_from_contacts <- cases_from_contacts(cases_clean = cases_clean)
#' }
cases_from_contacts <- function(cases_clean) {

  contacts_becoming_cases <- dplyr::filter(
    .data = cases_clean,
    .data$was_contact == TRUE
  )

  # set this status to became case and no longer active
  contacts_becoming_cases <- dplyr::mutate(
    .data = contacts_becoming_cases,
    follow_up_status = "BECAME_CASE",
    was_case = NA,
    date_of_last_contact = NA,
    follow_up_team_id = NA,
    relationship_exposure_type = NA,
    relationship_context_of_transmission = NA,
    relationship_exposure_duration = NA,
    relationship_exposure_frequency = NA,
    relationship_certainty_level = NA,
    relationship_cluster_id = NA,
  )

  # organize order of vars, only bring in what we need, take away confusing vars
  contacts_becoming_cases <- dplyr::select(
    .data = contacts_becoming_cases,
    "id", # identifier
    "visual_id", # identifier
    "classification", # identifier
    "follow_up_status", # identifier
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
    "risk_level", # epi
    "risk_reason", # epi
    "responsible_user_id", # assigned contact tracer
    "follow_up_team_id", # assigned contact tracer
    dplyr::matches("^admin_.*name$"), # address
    "lat", # address
    "long", # address
    "address", # address
    "postal_code", # address
    "city", # address
    "telephone", # address
    "email", # address
    "vaccinated",
    "outcome", # outcome
    "date_of_outcome", # outcome
    "relationship_exposure_type",
    "relationship_context_of_transmission",
    "relationship_exposure_duration",
    "relationship_exposure_frequency",
    "relationship_certainty_level",
    "relationship_cluster_id",
    "location_id", # uuid in case need later for joining of whatever sort
    "created_by", # record modification
    "datetime_created_at", # record modification
    "updated_by", # record modification
    "datetime_updated_at" # record modification
  )

  return(contacts_becoming_cases)
}

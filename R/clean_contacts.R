#' Clean contacts data
#'
#' @description Cleans and un-nests contact data. Contact data is returned by
#' [`get_contacts()`].
#'
#' @param contacts A `tibble` containing the contact data.
#' @param contacts_address_history_clean A `tibble` containing the cleaned
#' address history data from contacts (data is cleaned by
#' [`clean_contact_address_history()`].
#' @param contacts_vacc_history_clean A `tibble` containing the cleaned
#' vaccination history data from contacts (data is cleaned by
#' [`clean_contact_vax_history()`].
#' @param contacts_becoming_cases A `tibble` containing the cleaned data on
#' contacts that became cases (date is produced using
#' [`cases_from_contacts()`]).
#'
#' @return A `tibble` containing the cleaned case data.
#' @export
#'
#' @examples
#' \dontrun{
#'   url <- "https://MyGoDataServer.com/"
#'   username <- "myemail@email.com"
#'   password <- "mypassword"
#'   outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#'   contacts <- get_contacts(
#'     url = url,
#'     username = username,
#'     password = password,
#'     outbreak_id = outbreak_id
#'   )
#'
#'   locations <- get_locations(
#'     url = url,
#'     username = username,
#'     password = password
#'   )
#'
#'   locations_clean <- clean_locations(locations = locations)
#'
#'   # other cleaned data required for `clean_contacts()`
#'   contacts_vacc_history_clean <- clean_contact_vax_history(contacts = contacts)
#'   contacts_address_history_clean <- clean_contact_address_history(
#'     contacts = contacts,
#'     locations_clean = locations_clean
#'   )
#'
#'   cases <- get_cases(
#'     url = url,
#'     username = username,
#'     password = password,
#'     outbreak_id = outbreak_id
#'   )
#'   cases_address_history_clean <- clean_case_address_history(cases = cases)
#'   cases_vacc_history_clean <- clean_case_vax_history(cases = cases)
#'   cases_dateranges_history_clean <- clean_case_med_history(cases = cases)
#'
#'   cases_clean <- clean_cases(
#'     cases = cases,
#'     cases_address_history_clean = cases_address_history_clean,
#'     cases_vacc_history_clean = cases_vacc_history_clean,
#'     cases_dateranges_history_clean = cases_dateranges_history_clean
#'   )
#'   contacts_becoming_cases <- cases_from_contacts(cases_clean = cases_clean)
#'
#'   contacts_clean <- clean_contacts(
#'     contacts = contacts,
#'     contacts_address_history_clean = cases_address_history_clean,
#'     contacts_vacc_history_clean = cases_vacc_history_clean,
#'     contacts_becoming_cases = contacts_becoming_cases
#'   )
#' }
clean_contacts <- function(contacts,
                           contacts_address_history_clean,
                           contacts_vacc_history_clean,
                           contacts_becoming_cases) {

  # Remove all deleted records
  contacts_clean <- dplyr::filter(
    .data = contacts,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # Remove all nested fields, otherwise problems with exporting to excel
  contacts_clean <- dplyr::select_if(
    .tbl = contacts_clean,
    .predicate = purrr::negate(is.list)
  )

  # take out all that are not core variables, otherwise diff versions and
  # problems exporting to excel
  contacts_clean <- dplyr::select(
    .data = contacts_clean,
    -dplyr::contains("questionnaireAnswers")
  )

  # standardize column name syntax
  contacts_clean <- janitor::clean_names(dat = contacts_clean)

  # label timestamps as datetime
  contacts_clean <- dplyr::rename(
    .data = contacts_clean,
    date_of_birth = "dob",
    date_of_follow_up_start = "follow_up_start_date",
    date_of_follow_up_end = "follow_up_end_date",
    datetime_updated_at = "updated_at",
    datetime_created_at = "created_at"
  )

  # take out other unnecessary vars that are unnecessary and may confuse
  # (i.e. was_case for cases)
  contacts_clean <- dplyr::select(
    .data = contacts_clean,
    -c(
      "is_date_of_reporting_approximate",
      "was_contact",
      "follow_up_original_start_date",
      "type",
      "deleted",
      "created_on"
    )
  )

  #clean up all character fields
  contacts_clean <- dplyr::mutate(
    .data = contacts_clean,
    dplyr::across(dplyr::where(is.character), na_if, "")
  )

  # clean date formats (TODO: edit this so that we can see time stamps)
  contacts_clean <- dplyr::mutate_at(
    .tbl = contacts_clean,
    .vars = dplyr::vars(dplyr::starts_with("date_")),
    list(~ as.Date(substr(., 1, 10)))
  )

  contacts_clean <- dplyr::mutate(
    .data = contacts_clean,
    datetime_updated_at = as.POSIXct(datetime_updated_at, format = "%Y-%m-%dT%H:%M")
  )

  contacts_clean <- dplyr::mutate(
    .data = contacts_clean,
    datetime_created_at = as.POSIXct(datetime_created_at, format = "%Y-%m-%dT%H:%M")
  )

  #  truncate responses of categorical vars so easier to read
  contacts_clean <- dplyr::mutate(
    .data = contacts_clean,
    classification = sub(".*CLASSIFICATION_", "", classification),
    gender = sub(".*GENDER_", "", gender),
    occupation = sub(".*OCCUPATION_", "", occupation),
    outcome = sub(".*OUTCOME_", "", outcome_id),
    pregnancy_status = sub(".*STATUS_", "", pregnancy_status),
    risk_level = sub(".*LEVEL_", "", risk_level),
    follow_up_status = sub(".*TYPE_", "", follow_up_status),
    relationship_certainty_level = sub(".*LEVEL_", "", relationship_certainty_level_id),
    relationship_exposure_type = sub(".*TYPE_", "", relationship_exposure_type_id),
    relationship_context_of_transmission = sub(".*TRANSMISSION_", "", relationship_social_relationship_type_id),
    relationship_exposure_frequency = sub(".*FREQUENCY_", "", relationship_exposure_frequency_id),
    relationship_exposure_duration = sub(".*DURATION_", "", relationship_exposure_duration_id)
  )

  contacts_address_history_clean <- dplyr::filter(
    .data = contacts_address_history_clean,
    addresses_typeid == "USUAL_PLACE_OF_RESIDENCE"
  )

  # join in current address from address history, only current place of residence
  contacts_clean <- dplyr::left_join(
    x = contacts_clean,
    y = contacts_address_history_clean,
    by = "id"
  )

  # join in info from vacc block
  contacts_clean <- dplyr::mutate(
    .data = contacts_clean,
    vaccinated = case_when(id %in% contacts_vacc_history_clean$id[contacts_vacc_history_clean$vaccinesreceived_status == "VACCINATED"] ~ TRUE, TRUE ~ FALSE)
  )

  # force NA ages to appear as NA, not as 0 like sometimes occurs
  contacts_clean <- dplyr::mutate(.data = contacts_clean, age_years = as.numeric(age_years))
  contacts_clean <- dplyr::mutate(.data = contacts_clean, age_years = na_if(age_years,0))
  contacts_clean <- dplyr::mutate(.data = contacts_clean, age_months = as.numeric(age_months))
  contacts_clean <- dplyr::mutate(.data = contacts_clean, age_months = na_if(age_months,0))

  # standardize age vars into just one var, round by 1 decimal
  contacts_clean <- dplyr::mutate(
    .data = contacts_clean,
    age = case_when(!is.na(age_months) ~  round(age_months / 12, digits = 1),
                    TRUE ~ age_years))

  # WHO age categories updated Sept 2020:
  # 0-4, 5-9, 10-14, 15-19, 20-29, 30-39, 40-49, 50-59, 60-64, 65-69, 70-74,
  # 75-79, 80+
  # these categories below match that of detailed WHO surveillance dash:
  # <5, 5-14, 15-24, 25-64, 65+
  contacts_clean <- dplyr::mutate(
    .data = contacts_clean,
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
  contacts_clean <- dplyr::select(
    .data = contacts_clean,
    id, # identifier
    visual_id, # identifier
    classification, # identifier
    follow_up_status, # identifier
    first_name, # demographics
    middle_name, # demographics
    last_name, # demographics
    gender, # demographics
    age, # demographics
    age_class, # demographics
    occupation, # demographics
    pregnancy_status, # demographics
    date_of_reporting, # dates
    date_of_last_contact, # dates
    date_of_burial, # dates
    date_of_follow_up_start, # dates
    date_of_follow_up_end, # dates
    was_case, # epi
    risk_level, # epi
    risk_reason, # epi
    safe_burial, # epi
    transfer_refused, # epi
    responsible_user_id, # assigned contact tracer
    follow_up_team_id, # assigned contact tracer
    matches("^admin_.*name$"),
    lat, # address
    long, # address
    address, # address
    postal_code, # address
    city, # address
    telephone, # address
    email, # address
    vaccinated, # vaccination
    outcome, # outcome
    date_of_outcome,  # outcome
    relationship_exposure_type,
    relationship_context_of_transmission,
    relationship_exposure_duration,
    relationship_exposure_frequency,
    relationship_certainty_level,
    relationship_cluster_id,
    location_id = addresses_locationid,  # uuid in case need later for joining of whatever sort.
    created_by, # record modification
    datetime_created_at, # record modification
    updated_by, # record modification
    datetime_updated_at # record modification
  )

  #Join in cases that used to be contacts
  contacts_clean <- dplyr::bind_rows(contacts_clean, contacts_becoming_cases)

  return(contacts_clean)
}

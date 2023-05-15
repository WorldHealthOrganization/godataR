#' Cleans case data
#'
#' @description Cleans and un-nests case data. Case data is returned by
#' [`get_cases()`].
#'
#' @param cases A `tibble` containing the case data.
#' @param locations_clean A tibble with cleaned locations data. Locations data
#' is returned by [`get_locations()`] and cleaned by [`clean_locations()`].
#' @param language_tokens A tibble of language tokens returned by
#' [`get_language_tokens()`] to translate the string tokens in the data.
#'
#' @return A `tibble` containing the cleaned case data.
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
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = "english_us"
#' )
#'
#' locations <- get_locations(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#' locations_clean <- clean_locations(locations = locations)
#'
#' # other cleaned data required for `clean_cases()`
#' cases_vacc_history_clean <- clean_case_vax_history(
#'   cases = cases,
#'   language_tokens = language_tokens
#' )
#' cases_address_history_clean <- clean_case_address_history(
#'   cases = cases,
#'   locations_clean = locations_clean,
#'   language_tokens = language_tokens
#' )
#' cases_dateranges_history_clean <- clean_case_med_history(
#'   cases = cases,
#'   language_tokens = language_tokens
#' )
#'
#' cases_clean <- clean_cases(
#'   cases = cases,
#'   cases_address_history_clean = cases_address_history_clean,
#'   cases_vacc_history_clean = cases_vacc_history_clean,
#'   cases_dateranges_history_clean = cases_dateranges_history_clean,
#'   language_tokens = language_tokens
#' )
#' }
clean_cases <- function(cases,
                        cases_address_history_clean,
                        cases_vacc_history_clean,
                        cases_dateranges_history_clean,
                        language_tokens) {

  # Remove all deleted records
  cases_clean <- dplyr::filter(
    .data = cases,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # Remove all nested fields, otherwise problems with exporting to excel
  cases_clean <- dplyr::select_if(
    .tbl = cases_clean,
    purrr::negate(is.list)
  )

  # take out all that are not core variables, otherwise diff versions and
  # problems exporting to excel
  cases_clean <- dplyr::select(
    .data = cases_clean,
    -dplyr::contains("questionnaireAnswers")
  )

  # standardize column name syntax
  cases_clean <- janitor::clean_names(cases_clean)

  # label timestamps as datetime
  cases_clean <- dplyr::rename(
    .data = cases_clean,
    date_of_birth = "dob",
    datetime_updated_at = "updated_at",
    datetime_created_at = "created_at"
  )

  # take out other unnecessary vars that are unnecessary and may confuse
  # (i.e. was_case for cases)
  cases_clean <- dplyr::select(
    .data = cases_clean,
    -c("is_date_of_onset_approximate",
       "is_date_of_reporting_approximate",
       "was_case",
       "deleted",
       "created_on")
  )

  #clean up all blank fields
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    dplyr::across(dplyr::where(is.character), dplyr::na_if, "")
  )

  # clean date formats (TODO: edit this so that we can see time stamps)
  cases_clean <- dplyr::mutate_at(
    .tbl = cases_clean,
    dplyr::vars(dplyr::starts_with("date_")), list(~ as.Date(substr(., 1, 10)))
  )
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    datetime_updated_at = as.POSIXct(datetime_updated_at, format = "%Y-%m-%dT%H:%M")
  )
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")
  )

  # translate responses of categorical vars so easier to read
  cases_clean <- translate_categories(
    data = cases_clean,
    language_tokens = language_tokens
  )

  cases_clean <- dplyr::rename(
    .data = cases_clean,
    outcome = "outcome_id"
  )

  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    isolated = dplyr::case_when(id %in% cases_dateranges_history_clean$id[cases_dateranges_history_clean$dateranges_typeid == "Isolation"] ~ TRUE, TRUE ~ FALSE)
  )

  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    hospitalized = dplyr::case_when(id %in% cases_dateranges_history_clean$id[cases_dateranges_history_clean$dateranges_typeid == "Hospitalization"] ~ TRUE, TRUE ~ FALSE)
  )

  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    icu = dplyr::case_when(id %in% cases_dateranges_history_clean$id[cases_dateranges_history_clean$dateranges_typeid == "ICU Admission"] ~ TRUE, TRUE ~ FALSE)
  )

  cases_address_history_clean <- dplyr::filter(
    .data = cases_address_history_clean,
    addresses_typeid == "Current address"
  )

  # join in current address from address history, only current place of residence
  cases_clean <- dplyr::left_join(x = cases_clean, y = cases_address_history_clean, by = "id")

  # join in info from vacc block
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    vaccinated = dplyr::case_when(id %in% cases_vacc_history_clean$id[cases_vacc_history_clean$vaccinesreceived_status == "VACCINATED"] ~ TRUE, TRUE ~ FALSE)
  )

  # force NA ages to appear as NA, not as 0 like sometimes occurs
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    age_years = as.numeric(age_years)
  )
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    age_years = dplyr::na_if(age_years, 0)
  )
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    age_months = as.numeric(age_months)
  )
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    age_months = dplyr::na_if(age_months, 0)
  )

  # standardize age vars into just one var, round by 1 decimal
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    age = dplyr::case_when(!is.na(age_months) ~ round(age_months / 12, digits = 1), TRUE ~ age_years)
  )

  # WHO age categories updated Sept 2020:
  # 0-4, 5-9, 10-14, 15-19, 20-29, 30-39, 40-49, 50-59, 60-64, 65-69, 70-74,
  # 75-79, 80+
  # these categories below match that of detailed WHO surveillance dash:
  # <5, 5-14, 15-24, 25-64, 65+
  cases_clean <- dplyr::mutate(
    .data = cases_clean,
    age_class = factor(
      dplyr::case_when(
        age <= 4 ~ "<5",
        age <= 14 ~ "5-14",
        age <= 24 ~ "15-24",
        age <= 64 ~ "25-64",
        is.finite(age) ~ "65+",
        TRUE ~ "unknown"
      ),
      levels = c(
        "<5",
        "5-14",
        "15-24",
        "25-64",
        "65+",
        "unknown"
      )
    ),
    age_class = factor(
      age_class,
      levels = rev(levels(age_class))
    )
  )

  # organize order of vars, only bring in what we need, take away confusing vars
  cases_clean <- dplyr::select(
    .data = cases_clean,
    id, # identifier
    visual_id, # identifier
    classification, # identifier
    first_name, # demographics
    middle_name, # demographics
    last_name, # demographics
    gender, # demographics
    age, # demographics
    age_class, # demographics
    occupation, # demographics
    pregnancy_status, # demographics
    date_of_reporting, # dates
    date_of_onset, # dates
    date_of_infection, # dates
    date_become_case, # dates
    date_of_burial, # dates
    was_contact, # epi
    risk_level, # epi
    risk_reason, # epi
    safe_burial, # epi
    transfer_refused, # epi
    responsible_user_id, # assigned contact tracer
    matches("^admin_.*name$"), # address
    lat, # address
    long, # address
    address, # address
    postal_code, # address
    city, # address
    telephone, # address
    email, # address
    vaccinated, # vaccination & dateRanges block
    isolated, # vaccination & dateRanges block
    hospitalized, # vaccination & dateRanges block
    icu, # vaccination & dateRanges block
    outcome, # outcome
    date_of_outcome, # outcome
    location_id = addresses_locationid, # uuid in case need later for joining of whatever sort
    created_by, # record modification
    datetime_created_at, # record modification
    updated_by, # record modification
    datetime_updated_at # record modification
  )

  return(cases_clean)
}

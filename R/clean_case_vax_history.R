#' Cleans vaccination data from case data
#'
#' @description Cleans and un-nests vaccination history, where vaccination is
#' complete, from case data. Case data is returned from [`get_cases()`].
#'
#' @param cases A tibble with address information from cases data.
#'
#' @return A tibble with cleaned and un-nested vaccination history data.
#' @export
#'
#' @examples
#' #' \dontrun{
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
#' vax_history <- clean_case_vax_history(cases = cases)
#' }
clean_case_vax_history <- function(cases) {

  cases_vacc_history_clean <- dplyr::filter(
    .data = cases,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # cannot unnest on mix of data frames and lists so change empty lists to empty
  # data frames
  cases_vacc_history_clean$vaccinesReceived <- purrr::map(
    cases_vacc_history_clean$vaccinesReceived,
    .f = function(x) {
      if (length(x) == 0) x <- data.frame()
      x
    }
  )

  cases_vacc_history_clean <- tidyr::unnest(
    data = cases_vacc_history_clean,
    "vaccinesReceived",
    names_sep = "_"
  )

  cases_vacc_history_clean <- dplyr::select(
    .data = cases_vacc_history_clean,
    "id",
    "visualId",
    dplyr::starts_with("vaccinesReceived")
  )

  cases_vacc_history_clean <- dplyr::rename_with(
    .data = cases_vacc_history_clean,
    .fn = tolower
  )

  cases_vacc_history_clean <- dplyr::mutate(
    .data = cases_vacc_history_clean,
    vaccinesreceived_vaccine = sub(".*VACCINE_", "", vaccinesreceived_vaccine)
  )

  cases_vacc_history_clean <- dplyr::mutate(
    .data = cases_vacc_history_clean,
    vaccinesreceived_status = sub(".*STATUS_", "", vaccinesreceived_status)
  )

  cases_vacc_history_clean <- dplyr::mutate(
    .data = cases_vacc_history_clean,
    dplyr::across("vaccinesreceived_date", as.Date)
  )

  return(cases_vacc_history_clean)
}

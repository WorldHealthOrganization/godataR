#' Cleans vaccination data from contact data
#'
#' @description Cleans and un-nests vaccination history, where vaccination is
#' complete, from contact data. Contact data is returned from
#' [`get_contacts()`].
#'
#' @param contacts A tibble with address information from contact data.
#'
#' @return A tibble with cleaned and un-nested vaccination history data.
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
#' vax_history <- clean_contact_vax_history(contacts = contacts)
#' }
clean_contact_vax_history <- function(contacts) {

  contacts_vax_history_clean <- dplyr::filter(
    .data = contacts,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # cannot unnest on mix of data frames and lists so change empty lists to empty
  # data frames
  contacts_vax_history_clean$vaccinesReceived <- purrr::map(
    contacts_vax_history_clean$vaccinesReceived,
    .f = function(x) {
      if (length(x) == 0) x <- data.frame()
      x
    }
  )

  contacts_vax_history_clean <- tidyr::unnest(
    data = contacts_vax_history_clean,
    cols = "vaccinesReceived",
    names_sep = "_"
  )

  contacts_vax_history_clean <- dplyr::select_at(
    .tbl = contacts_vax_history_clean,
    dplyr::vars(
      "id",
      "visualId",
      dplyr::starts_with("vaccinesReceived")
    ),
    .funs = tolower
  )

  contacts_vax_history_clean <- dplyr::mutate(
    .data = contacts_vax_history_clean,
    vaccinesreceived_vaccine = sub(".*VACCINE_", "", vaccinesreceived_vaccine)
  )

  contacts_vax_history_clean <- dplyr::mutate(
    .data = contacts_vax_history_clean,
    vaccinesreceived_status = sub(".*STATUS_", "", vaccinesreceived_status)
  )

  contacts_vax_history_clean <- dplyr::mutate_at(
    .tbl = contacts_vax_history_clean,
    dplyr::vars(vaccinesreceived_date), as.Date
  )

  return(contacts_vax_history_clean)
}

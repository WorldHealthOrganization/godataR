#' Cleans vaccination data from contacts of contacts data
#'
#' @description Cleans and un-nests vaccination history, where vaccination is
#' complete, from contacts of contacts data. Contacts of contacts data is
#' returned from [`get_contacts_of_contacts()`].
#'
#' @param contacts_of_contacts A `tibble` with address information from contacts
#' of contacts data.
#' @param language_tokens A tibble of language tokens returned by
#' [`get_language_tokens()`] to translate the string tokens in the data.
#'
#' @return A `tibble` with cleaned and un-nested vaccination history data.
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
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = "english_us"
#' )
#'
#' vax_history <- clean_contacts_of_contacts_vax_history(
#'   contacts_of_contacts = contacts_of_contacts,
#'   language_tokens = language_tokens
#' )
#' }
clean_contacts_of_contacts_vax_history <- function(contacts_of_contacts,
                                                   language_tokens) {

  coc_vacc_hist <- dplyr::filter(
    .data = contacts_of_contacts,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  coc_vacc_hist <- tidyr::unnest(
    data = coc_vacc_hist,
    cols = "vaccinesReceived",
    names_sep = "_"
  )

  coc_vacc_hist <- dplyr::select_at(
    .tbl = coc_vacc_hist,
    .vars = dplyr::vars(id, visualId, dplyr::starts_with("vaccinesReceived")),
    tolower
  )

  coc_vacc_hist <- translate_categories(
    data = coc_vacc_hist,
    language_tokens = language_tokens
  )

  coc_vacc_hist <- dplyr::mutate_at(
    .tbl = coc_vacc_hist,
    dplyr::vars(vaccinesreceived_date),
    as.Date
  )

  return(coc_vacc_hist)
}

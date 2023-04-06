#' Cleans vaccination data from contact data
#'
#' @description Cleans and un-nests vaccination history, where vaccination is
#' complete, from contact data. Contact data is returned from
#' [`get_contacts()`].
#'
#' @param contacts A tibble with address information from contact data.
#' @param language_tokens A tibble of language tokens returned by
#' [`get_language_tokens()`] to translate the string tokens in the data.
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
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = "english_us"
#' )
#'
#' vax_history <- clean_contact_vax_history(
#'   contacts = contacts,
#'   language_tokens = language_tokens
#' )
#' }
clean_contact_vax_history <- function(contacts,
                                      language_tokens) {

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

  contacts_vax_history_clean <- translate_categories(
    data = contacts_vax_history_clean,
    language_tokens = language_tokens
  )

  contacts_vax_history_clean <- dplyr::mutate_at(
    .tbl = contacts_vax_history_clean,
    dplyr::vars(vaccinesreceived_date), as.Date
  )

  return(contacts_vax_history_clean)
}

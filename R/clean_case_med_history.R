#' Extracts and cleans medical history from case data
#'
#' @description This function un-nests and cleans date ranges of isolation and
#' hospitalization history and stores it in a standalone table.
#'
#' @param cases A tibble with case data. Case data is returned by
#' [`get_cases()`].
#' @param language_tokens A tibble of language tokens returned by
#' [`get_language_tokens()`] to translate the string tokens in the data.
#'
#' @return A tibble with information on isolation and hospitalization history.
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
#' cases_med_history <- clean_case_med_history(
#'   cases = cases,
#'   language_tokens = language_tokens
#' )
#' }
clean_case_med_history <- function(cases,
                                   language_tokens) {

  cases_dateranges_history_clean <- dplyr::filter(
    .data = cases,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # cannot unnest on mix of data frames and lists so change empty lists to empty
  # data frames
  cases_dateranges_history_clean$dateRanges <- purrr::map(
    cases_dateranges_history_clean$dateRanges,
    .f = function(x) {
      if (length(x) == 0) x <- data.frame()
      x
    }
  )

  cases_dateranges_history_clean <- tidyr::unnest(
    data = cases_dateranges_history_clean,
    cols = dateRanges,
    names_sep = "_"
  )

  cases_dateranges_history_clean <- dplyr::select_at(
    .tbl = cases_dateranges_history_clean,
    dplyr::vars(id, visualId, starts_with("dateRanges")),
    tolower
  )

  cases_dateranges_history_clean <- translate_categories(
    data = cases_dateranges_history_clean,
    language_tokens = language_tokens
  )

  cases_dateranges_history_clean <- dplyr::mutate_at(
    .tbl = cases_dateranges_history_clean,
    dplyr::vars(dateranges_startdate, dateranges_enddate),
    as.Date
  )

  cases_dateranges_history_clean <- dplyr::select_if(
    .tbl = cases_dateranges_history_clean,
    purrr::negate(is.list)
  )

  return(cases_dateranges_history_clean)
}

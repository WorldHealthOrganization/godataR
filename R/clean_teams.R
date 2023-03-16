#' Clean teams data
#'
#' @description Cleans and un-nests teams data. Teams data is returned by
#' [`get_teams()`].
#'
#' @param teams A `tibble` containing teams data. Teams data is returned by
#' [`get_teams()`].
#'
#' @return A `tibble` of cleaned teams data
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' teams <- get_teams(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#'
#' clean_teams <- clean_teams(teams)
#' }
clean_teams <- function(teams) {

  # Remove all deleted records
  clean_teams <- dplyr::filter(
    .data = teams,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # standardize column name syntax
  clean_teams <- janitor::clean_names(clean_teams)

  # label timestamps as datetime
  clean_teams <- dplyr::rename(
    .data = clean_teams,
    datetime_updated_at = "updated_at",
    datetime_created_at = "created_at"
  )

  #clean up all character fields
  clean_teams <- dplyr::mutate(
    .data = clean_teams,
    dplyr::across(dplyr::where(is.character), na_if, "")
  )

  clean_teams <- tidyr::unnest_wider(
    data = clean_teams,
    col = "user_ids",
    names_sep = "_"
  )
  clean_teams <- tidyr::unnest_wider(
    data = clean_teams,
    col = "location_ids",
    names_sep = "_"
  )

  # organize order of vars, only bring in what we need, take away confusing vars
  clean_teams <- dplyr::select(
    .data = clean_teams,
    "id",
    "name",
    dplyr::starts_with("user_ids"),
    dplyr::starts_with("location_ids"),
    "created_by", # record modification
    "datetime_created_at", # record modification
    "updated_by", # record modification
    "datetime_updated_at" # record modification
  )

  return(clean_teams)
}

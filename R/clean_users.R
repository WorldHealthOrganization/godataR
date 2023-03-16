#' Cleans users data
#'
#' @description Cleans and un-nests users data. Users data is returned by
#' [`get_users()`].
#'
#' @param users A `tibble` containing users data. Users data is returned by
#' [`get_users()`].
#'
#' @return A `tibble` with cleaned users data.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' users <- get_users(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#'
#' clean_users <- clean_users(users)
#' }
clean_users <- function(users) {

  # standardize column name syntax
  clean_users <- janitor::clean_names(users)

  # label timestamps as datetime
  clean_users <- dplyr::rename(
    clean_users,
    datetime_last_login = "last_login_date",
    datetime_created_at = "created_at"
  )

  # clean up all character fields
  clean_users <- dplyr::mutate(
    .data = clean_users,
    dplyr::across(dplyr::where(is.character), dplyr::na_if, "")
  )

  clean_users <- tidyr::unnest_wider(clean_users, "role_ids", names_sep = "_")

  #  truncate responses of categorical vars so easier to read
  clean_users <- dplyr::mutate(
    clean_users,
    institution_name = sub(".*NAME_", "", .data$institution_name)
  )

  # organize order of vars, only bring in what we need, take away confusing vars
  clean_users <- dplyr::select(
    .data = clean_users,
    "id",
    "first_name",
    "last_name",
    "email",
    "institution_name",
    "disregard_geographic_restrictions",
    dplyr::starts_with("role_ids"),
    "active_outbreak_id",
    "created_by",
    "datetime_created_at",
    "datetime_last_login"
  )

  return(clean_users)
}

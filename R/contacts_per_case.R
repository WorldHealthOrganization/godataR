#' Counts the number of contacts per case from relationship data
#'
#' @description Uses cleaned relationship data to tally the number of contacts
#' per case. Relationship data is returned by [`get_relationships()`] and
#' cleaned by [`clean_relationships()`].
#'
#' @param relationships_clean A `tibble` with the cleaned relationship data.
#' Relationship data is returned by [`get_relationships()`] and cleaned by
#' [`clean_relationships()`].
#'
#' @return A `tibble` with the number of contacts associated to each source
#' person
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' relationships <- get_relationships(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#'
#' clean_relationships <- clean_relationships(relationships)
#'
#' contacts_per_case <- contacts_per_case(clean_relationships)
#' }
contacts_per_case <- function(relationships_clean) {

  contacts_per_case <- dplyr::group_by(
    .data = relationships_clean,
    .data$source_person_id
  )

  contacts_per_case <- dplyr::tally(x = contacts_per_case)

  contacts_per_case <- dplyr::select(
    .data = contacts_per_case,
    "source_person_id",
    no_contacts = n
  )

  return(contacts_per_case)
}

#' Counts the number of exposures per case from relationship data
#'
#' @description Uses cleaned relationship data to tally the number of contacts
#' per case. Relationship data is returned by [`get_relationships()`] and
#' cleaned by [`clean_relationships()`].
#'
#' @param relationships_clean A `tibble` with the cleaned relationship data.
#' Relationship data is returned by [`get_relationships()`] and cleaned by
#' [`clean_relationships()`].
#'
#' @return A `tibble` with the number of exposures associated to each target
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
#' exposures_per_case <- exposures_per_case(clean_relationships)
#' }
exposures_per_case <- function(relationships_clean) {

  exposures_per_case <- dplyr::group_by(
    .data = relationships_clean,
    .data$target_person_id
  )

  exposures_per_case <- dplyr::tally(x = exposures_per_case)

  exposures_per_case <- dplyr::select(
    .data = exposures_per_case,
    "target_person_id",
    no_exposures = n
  )

  return(exposures_per_case)
}

#' Cleans relationship data
#'
#' @description Cleans and un-nests relationship data. Relationship data is
#' returned by [`get_relationships()`].
#'
#' @param relationships A `tibble` of relationship data. Relationship data is
#' returned by [`get_relationships()`].
#'
#' @return A `tibble` with clean relationship data.
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
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = "english_us"
#' )
#'
#' clean_relationships <- clean_relationships(
#'   relationships,
#'   language_tokens = language_tokens
#' )
#' }
clean_relationships <- function(relationships,
                                language_tokens) {

  # Remove all deleted records
  clean_relationships <- dplyr::filter(
    .data = relationships,
    .data$deleted == FALSE | is.na(.data$deleted)
  )

  # Remove all nested fields, otherwise problems with exporting to excel
  clean_relationships <- dplyr::select_if(
    .tbl = clean_relationships,
    purrr::negate(is.list)
  )

  # standardize column name syntax
  clean_relationships <- janitor::clean_names(clean_relationships)

  # label timestamps as datetime
  clean_relationships <- dplyr::rename(
    .data = clean_relationships,
    datetime_updated_at = "updated_at",
    datetime_created_at = "created_at"
  )

  #clean up all character fields
  clean_relationships <- dplyr::mutate(
    .data = clean_relationships,
    dplyr::across(dplyr::where(is.character), na_if, "")
  )

  # clean date formats (TODO: edit this so that we can see time stamps)
  clean_relationships <- dplyr::mutate(
    .data = clean_relationships,
    dplyr::across(
      dplyr::starts_with("date_"), list(~ as.Date(substr(., 1, 10)))
    )
  )
  clean_relationships <- dplyr::mutate(
    .data = clean_relationships,
    datetime_updated_at = as.POSIXct(
      datetime_updated_at,
      format = "%Y-%m-%dT%H:%M"
    )
  )
  clean_relationships <- dplyr::mutate(
    .data = clean_relationships,
    datetime_created_at = as.POSIXct(
      datetime_created_at,
      format = "%Y-%m-%dT%H:%M"
    )
  )

  # translate responses of categorical vars so easier to read
  clean_relationships <- translate_categories(
    data = clean_relationships,
    language_tokens = language_tokens
  )

  # organize order of vars, only bring in what we need, take away confusing vars
  clean_relationships <- dplyr::select(
    .data = clean_relationships,
    "id", #id
    "source_person_id", #id
    "source_person_visual_id", #id
    "target_person_id", #id
    "target_person_visual_id", #id
    "source_person_type", #id
    "target_person_type", #id
    "created_by", # record modification
    "datetime_created_at", # record modification
    "updated_by", # record modification
    "datetime_updated_at" # record modification
  )

  return(clean_relationships)
}

#' Add new cases to Go.Data
#'
#' A function to add new cases to a specific `outbreak_id`.
#'
#' This function creates new records for cases. The data must be provided as a list object. Field names must match those used in Go.Data. You can use `get_data_model()` to determine appropriate field names.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to add data.
#' @param data The dataframe (as a list object) to import into Go.Data. See Details.
#'
#' @return
#' Returns the status of the request.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' add_cases(url=url,
#'           username=username,
#'           password=password,
#'           outbreak_id=outbreak_id)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck


add_cases <- function(url, username, password, outbreak_id, data) {

  post_request <- POST(paste0(url,"api/outbreaks/",outbreak_id,"/cases"),
                       add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " ")),
                       content_type("application/json"),
                       body = data,
                       encode = "json")

  return(post_request)
}

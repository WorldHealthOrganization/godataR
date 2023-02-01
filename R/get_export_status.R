#' Check the status of an export request from Go.Data (version 2.38.1 or later)
#'
#' A function to check the status of an export
#' request. This is a housekeeping function
#' used in many of the other `godataR` functions.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param request_id The id number for the export request.
#'
#' @return
#' Returns the current status of the export request.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#' access_token <- get_access_token(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#'
#' # Submit an export request
#' export.request <- GET(
#'   paste0(
#'     url,
#'     "api/outbreaks/",
#'     outbreak_id,
#'     "/cases/export",
#'     "&access_token=",
#'     access_token
#'   )
#' )
#' request_id <- export.request %>%
#'   content() %>%
#'   pluck("exportLogId")
#'
#' # Check the status of the export request
#'
#' export_request_status <- get_export_status(
#'   url = url,
#'   username = username,
#'   password = password,
#'   request_id = request_id
#' )
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck

get_export_status <- function(url = url,
                              username = username,
                              password = password,
                              request_id = request_id) {

  export_request_status <- GET(
    paste0(
      url,
      "api/export-logs/",
      request_id,
      "?access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  ) %>%
    content()

  export_request_status <- export_request_status[
    c("statusStep", "totalNo", "processedNo")
  ]

  return(export_request_status)

}

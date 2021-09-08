#' Check the status of an export request from Go.Data (version 2.38.1 or later)
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to download cases.
#' @param wait The number of seconds to wait in between iterations of checking the status of the download. Default is 5 seconds, but the user can specify a smaller value to speed up the process if the dataset is small.
#'
#' @return
#' Returns data frame of cases. Some fields, such as addresses, hospitalization history, and questionnaire fields will require further unnesting. See the tidyr::unnest() function.
#' @export
#' @examples
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' #Submit an export request
#' export.request <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases/export",
#'                                "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
#'                                "&access_token=",get_access_token(url=url, username=username, password=password)))
#' request_id <- export.request %>%
#'      content() %>%
#'      pluck("exportLogId")
#'
#' #Check the status of the export request
#'
#' cases <- get_cases2(url=url, username=username, password=password, outbreak_id=outbreak_id)
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck

get_export_status <- function(url=url, username=username, password=password, request_id=request_id) {

  export.request.status <- GET(paste0(url,"api/export-logs/",request_id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content()

  export.request.status <- export.request.status[c("statusStep","totalNo","processedNo")]

  return(export.request.status)

}

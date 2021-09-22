#' Download relationships from Go.Data (version 2.38.1 or later)
#'
#' A function to retrieve the relationship data for
#' a specific `outbreak_id`. This function relies
#' on the `/outbreaks/{id}/lab-results/export`
#' API endpoint.
#'
#' If `file.type="json"`, then some fields, such as addresses, hospitalization history, and questionnaire fields may require further unnesting. See the `tidyr::unnest()` function.
#'
#' If `file.type="csv"`, then all fields will be unnested resulting in a greater number of fields.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to download cases.
#' @param wait The number of seconds to wait in between iterations of checking the status of the download. Default is 5 seconds, but the user can specify a smaller value to speed up the process if the dataset is small.
#' @param file.type Whether the API should return a data structure with nested fields (json, the default) or an entirely flat data structure (csv)
#'
#' @return
#' Returns data frame of all created relationships.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' relationships <- get_relationships2(url=url,
#'                                     username=username,
#'                                     password=password,
#'                                     outbreak_id=outbreak_id)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @import utils

get_relationships2 <- function(url=url,
                               username=username,
                               password=password,
                               outbreak_id=outbreak_id,
                               wait=5,
                               file.type=c("json","csv")) {

  #Check version of Go.Data
  if (check_godata_version(url=url)==FALSE) {
    stop("Go.Data must be version 2.38.1 or later. Please use the function get_cases() instead.")
  }

  #Check that outbreak_id is active
  if (outbreak_id != get_active_outbreak(url=url, username=username, password=password)) {
    set_active_outbreak(url=url, username=username, password=password, outbreak_id=outbreak_id)
  }

  #Default value of file.type is "json"
  if (missing(file.type)) file.type <- "json"

  #Submit an export request to the system
  if (file.type=="json") {
    request_id <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/relationships/export",
                             "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                             "&access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content() %>%
      pluck("exportLogId")
  } else if (file.type=="csv") {
    request_id <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/replationships/export",
                             "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                             "&type=csv",
                             "&access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content() %>%
      pluck("exportLogId")
  }

  #Check status of request periodcially, until finished
  #function argument 'wait' determines the number of seconds to wait between iterations
  export.request.status <- get_export_status(url=url, username=username, password=password, request_id=request_id)

  while(export.request.status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
    Sys.sleep(wait)
    export.request.status <- GET(paste0(url,"api/export-logs/",request_id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content()
    message(paste0("...processed ",export.request.status$processedNo, " of ", export.request.status$totalNo, " records"))
  }

  #Download the export
  message("...beginning download")
  if (file.type=="json") {
    df <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content("text", encoding="UTF-8") %>%
      fromJSON(flatten=TRUE)

    # fix one strange variable name
    names(df)[names(df) %in% "_id"] <- "id"
  } else if (file.type=="csv") {
    df <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content("text", encoding="UTF-8") %>%
      textConnection() %>%
      read.csv()
    names(df)[names(df) %in% "X_id"] <- "id"
  }

  message("...download complete!")
  return(df)
}

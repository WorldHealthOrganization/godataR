#' Function to manage export downloads
#'
#' A housekeeping function to do export requests & downloads.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param api_call_request The API url to get the number of records.
#' @param wait The number of seconds to wait in between iterations of checking the status of the export.
#' @param file.type Whether the resulting data frame should contain nested fields (`file.type="json"`, the default) or an entirely flat data structure (`file.type="csv"`)

#' @return
#' Returns a data frame. Some fields, such as addresses, hospitalization history, and questionnaire fields will require further unnesting. See the \code{\link[tidyr]{unnest}} function.
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' cases <- get_cases(url=url,
#'                    username=username,
#'                    password=password,
#'                    outbreak_id=outbreak_id)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom utils read.csv
#'
#'
export_downloader <- function(url = url,
                             username = username,
                             password = password,
                             api_call_request = api_call_request,
                             wait = wait,
                             file.type = file.type) {

  request_id <- GET(paste0(api_call_request,
                           "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                           "&type=",file.type,
                           "&access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content() %>%
    pluck("exportLogId")


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



#' Function to manage export downloads
#'
#' A housekeeping function to do export requests & downloads.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param api_call_request The API url to get the number of records.
#' @param wait The number of seconds to wait in between iterations of checking
#' the status of the export.
#' @param file_type Whether the resulting data frame should contain nested
#' fields (`file_type = "json"`, the default) or an entirely flat data structure
#' (`file_type = "csv"`)
#'
#' @return
#' Returns a data frame. Some fields, such as addresses, hospitalization
#' history, and questionnaire fields may require further unnesting. See
#' `\link[tidyr]{nest}` for assitance with unnesting.
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' cases <- get_cases(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom utils read.csv
export_downloader <- function(url,
                              username,
                              password,
                              api_call_request,
                              wait,
                              file_type) {

  export_log_id_request <- GET(
    paste0(
      api_call_request,
      "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22",
      "dontTranslateValues%22%3A%22true%22%2C%20%22",
      "jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
      "&type=",
      file_type,
      "&access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )

  export_log_id_request_content <- content(export_log_id_request)

  request_id <- pluck(export_log_id_request_content, "exportLogId")

  #Check status of request periodcially, until finished
  #function argument 'wait' determines the number of seconds to wait between
  #iterations
  export_request_status <- get_export_status(
    url = url,
    username = username,
    password = password,
    request_id = request_id
  )

  status_step <- export_request_status$statusStep
  while (status_step != "LNG_STATUS_STEP_EXPORT_FINISHED") {
    Sys.sleep(wait)
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
    )

    export_request_status_content <- content(export_request_status)
    message(
      paste0(
        "...processed ",
        export_request_status$processedNo,
        " of ",
        export_request_status$totalNo,
        " records"
      )
    )
  }

  #Download the export
  message("...beginning download")
  if (file_type == "json") {
    df_request <- GET(
      paste0(
        url,
        "api/export-logs/",
        request_id,
        "/download?access_token=",
        get_access_token(
          url = url,
          username = username,
          password = password
        )
      )
    )

    df_content <- content(df_request, "text", encoding = "UTF-8")

    df <- fromJSON(df_content, flatten = TRUE)

    # fix one strange variable name
    names(df)[names(df) %in% "_id"] <- "id"
  } else if (file_type == "csv") {
    df_request <- GET(
      paste0(
        url,
        "api/export-logs/",
        request_id,
        "/download?access_token=",
        get_access_token(
          url = url,
          username = username,
          password = password
        )
      )
    )

    df_content <- content(df_request, "text", encoding = "UTF-8")

    df_content <- textConnection(df_content)

    df <- read.csv(df_content)
    names(df)[names(df) %in% "X_id"] <- "id"
  }

  message("...download complete!")
  return(df)
}

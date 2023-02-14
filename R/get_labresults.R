#' Download lab results from Go.Data
#'
#' A function to retrieve the lab result data for a
#' specific `outbreak_id`.
#'
#' This function works on all versions of Go.Data. There
#' are two methods for downloading the data:
#'
#' `method="batches"` will work on all versions of
#'    Go.Data. This method relies on the GET outbreak/{id}/lab-results
#'    API endpoint. Records are then retrieved in batches
#'    based on `batch_size` and appended together into
#'    a final dataset. `method="batches"` will be the default and
#'    only available method for Go.Data version 2.38.0 or older.
#'
#' `method="export"` will only work on Go.Data versions
#'    2.38.1 or newer. This method relies on the GET
#'    outbreak/{id}/lab-results/export API endpoint. An export
#'    request is submitted to the server, and then when the
#'    export is ready, it will be downloaded. Due to better
#'    performance and more options, `method="export"` will
#'    be the default if you are using Go.Data version 2.38.1
#'    or newer.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to
#' download data.
#' @param method The method to download data. `method="export"` is the
#' preferred and default method for Go.Data version 2.38.1 or later. See
#' Details.
#' @param batch_size If `method = "batches"`, then `batch_size` specifies the
#' number of records to retrieve in each iteration.
#' @param wait If `method = "export"`, then `wait` is the number of seconds to
#' wait in between iterations of checking the status of the export.
#' @param file_type If `method = "export"`, then `file_type` determines Whether
#' the resulting data frame should contain nested fields (`file_type = "json"`,
#' the default) or an entirely flat data structure (`file_type = "csv"`)
#'
#' @return
#' Returns a data frame. Some fields, such as addresses, hospitalization
#' history, and questionnaire fields may require further unnesting. See
#' \code{\link[tidyr]{nest}} for assitance with unnesting.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' labresults <- get_labresults(
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
get_labresults <- function(url,
                           username,
                           password,
                           outbreak_id,
                           method = c("export", "batch"),
                           batch_size = 50000,
                           wait = 2,
                           file_type = c("json", "csv")) {

  #Check that outbreak_id is active
  active_outbreak <- get_active_outbreak(
    url = url,
    username = username,
    password = password
  )
  if (outbreak_id != active_outbreak) {
    set_active_outbreak(
      url = url,
      username = username,
      password = password,
      outbreak_id = outbreak_id
    )
  }

  #Set default method based on current version of Go.Data
  version_check <- check_godata_version(url = url)
  if (!version_check) {
    method <- "batches" #Older version of Go.Data can only use the batch method
  } else if (missing(method)) {
    method <- "export" # For new versions of Go.Data, default to export method
  }


  if (method == "batches") {

    api_call_n <- paste0(
      url,
      "api/outbreaks/",
      outbreak_id,
      "/lab-results/aggregate-filtered-count"
    )
    api_call_get <- paste0(
      url,
      "api/outbreaks/",
      outbreak_id,
      "/lab-results/aggregate"
    )
    df <- batch_downloader(
      url = url,
      username = username,
      password = password,
      api_call_n = api_call_n,
      api_call_get = api_call_get,
      batch_size = batch_size
    )

  } else if (method == "export") {

    #Default value of file_type is "json"
    if (missing(file_type)) file_type <- "json"

    #Submit an export request to the system
    api_call_request <- paste0(
      url,
      "api/outbreaks/",
      outbreak_id,
      "/lab-results/export"
    )
    df <- export_downloader(
      url = url,
      username = username,
      password = password,
      api_call_request = api_call_request,
      file_type = file_type,
      wait = wait
    )
  }

  return(df)
}

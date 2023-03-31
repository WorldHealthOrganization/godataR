#' Download cases from Go.Data and returns questionnaire fields
#'
#' A function that retrieves the questionnaire fields from case data for a
#' specific `outbreak_id`.
#'
#' Unlike [`get_cases()`] this function only uses the [`export_downloader()`],
#' and not the [`batch_downloader()`]. Therefore, this function will only work
#' on Go.Data versions 2.38.1 or newer. This method relies on the GET
#' outbreak/{id}/cases/export API endpoint. An export request is submitted to
#' the server, and then when the export is ready, it will be downloaded.
#'
#' This function fixes the file return type to `"csv"`.
#'
#' @param url Insert the base URL for your instance of Go.Data here.
#' Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to
#' download data.
#' @param method The method to download data. `method = "export"` is the
#' preferred and default method for Go.Data version 2.38.1 or later.
#' See Details.
#' @param batch_size If `method = "batches"`, then `batch_size` specifies the
#' number of records to retrieve in each iteration.
#' @param wait If `method = "export"`, then `wait` is the number of seconds to
#' wait in between iterations of checking the status of the export.
#' @param file_type If `method = "export"`, then `file_type` determines Whether
#' the resulting data frame should contain nested fields (`file_type = "json"`,
#' the default) or an entirely flat data structure (`file_type = "csv"`)
#'
#' @return Returns a `tibble`.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' cases <- get_cases_questionnaire(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#' }
get_cases_questionnaire <- function(url,
                                    username,
                                    password,
                                    outbreak_id,
                                    wait = 2) {

  #Check that outbreak_id is active
  active_outbreak_id <- get_active_outbreak(
    url = url,
    username = username,
    password = password
  )
  if (outbreak_id != active_outbreak_id) {
    set_active_outbreak(
      url = url,
      username = username,
      password = password,
      outbreak_id = outbreak_id
    )
  }

  #Submit an export request to the system
  api_call_request <- paste0(
    url, "api/outbreaks/", outbreak_id, "/cases/export"
  )
  df <- export_downloader(
    url = url,
    username = username,
    password = password,
    api_call_request = api_call_request,
    file_type = "csv",
    wait = wait
  )

  # subset columns to questionnaire
  df_questionnaire <- df[, grep(pattern = "FA", x = colnames(df))]

  return(tibble::as_tibble(df_questionnaire))
}

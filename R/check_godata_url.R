#' Check if the provided Go.Data URL is valid
#'
#' A function to check whether the provided URL
#' is valid. This is a housekeeping function
#' used in many of the other `godataR` functions.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param success_code A numeric specifying which code is returned by the API
#' when successfully returning the status code. Default is 200.
#'
#' @return
#' Boolean, where `TRUE` indicates a valid URL.
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' check_godata_url(url = url)
#' }
#' @export
check_godata_url <- function(url,
                             success_code = 200) {

  # Get status code for version check
  status_code <- httr::GET(paste0(url, "api/system-settings/version"))

  status_code <- purrr::pluck(status_code, "status_code")

  # return boolean based on status code being a success
  return(isTRUE(status_code == success_code))

}

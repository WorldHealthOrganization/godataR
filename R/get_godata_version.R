#' Quickly check which verison of Go.Data is currently installed.
#'
#' A function to retrieve the current version
#' of Go.Data on the provided URL. This is a
#' housekeeping function used in many of the
#' other `godataR` functions.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#'
#' @return string
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' get_godata_version(url = url)
#' }
get_godata_version <- function(url = url) {

  version_request <- httr::GET(paste0(url, "api/system-settings/version"))

  if (version_request$status_code == 200) {
    version <- httr::content(version_request, as = "text")
    version <- jsonlite::fromJSON(version, flatten = TRUE)
    return(version$version)
  } else {
    stop(paste0("Error ", version_request$status_code))
  }
}

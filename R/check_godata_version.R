#' Check if installed verison of Go.Data is 2.38.1 or later
#'
#' A function to check whether the current
#' version of Go.Data on the provided URL.
#' This is a housekeeping function used in
#' many of the other `godataR` functions.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#'
#' @return Boolean, where `TRUE` indicates version 2.38.1 or later.
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' check_godata_version(url = url)
#' }
check_godata_version <- function(url = url) {

  # Get Current Version of Go.Data
  gd_version <- get_godata_version(url = url)

  # Convert string to vector of 3 numbers
  gd_version <- stringr::str_split(gd_version, "[.]")

  gd_version <- as.numeric(unlist(gd_version))

  # Check if 2.38.1 or later
  # Should be TRUE if it is version 2.38.1 or later &
  # FALSE if version 2.38.0 or earlier
  if (gd_version[1] < 2) {
    return(FALSE)
  } else if (gd_version[1] == 2 && gd_version[2] < 38) {
    return(FALSE)
  } else if (gd_version[1] == 2 && gd_version[2] == 38 && gd_version[3] == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

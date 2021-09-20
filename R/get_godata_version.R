#' Quickly check which verison of Go.Data is currently installed.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#'
#' @return string
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' get_godata_version(url=url)
#' }
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%

get_godata_version <- function(url=url) {

  version.request <- GET(paste0(url,"api/system-settings/version"))

  if (version.request$status_code==200) {
    version <- content(version.request, as="text") %>%
      fromJSON(flatten=TRUE)
    return(version$version)
  } else {
    stop(paste0("Error ",version.request$status_code))
  }

}

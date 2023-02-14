#' Get location data from Go.Data
#'
#' A function to retreive a list of all locations
#' and their attributes, across all outbreaks on
#' the instance (since this is a system-level API
#' endpoint). This function relies on the `\locations`
#' API endpoint.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns a data frame of locations associated with Go.Data instance.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' locations <- get_locations(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @export


get_locations <- function(url = url,
                          username = username,
                          password = password) {

  locations <- GET(
    paste0(
      url,
      "api/locations",
      "?access_token=",
      godataR::get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  ) %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    filter(.data$deleted != TRUE)

  return(locations)

}

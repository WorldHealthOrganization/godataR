#' Get reference data from Go.Data
#'
#' A function to retreive a list of all reference
#' data and their attributes, across all outbreaks
#' on the instance (since this is a system-level API
#' endpoint). This function relies on the
#' `\reference-data` API endpoint.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns data frame of reference data associated with Go.Data instance.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' reference_data <- get_reference_data(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#' }
#' @export
get_reference_data <- function(url,
                               username,
                               password) {

  reference_data_request <- httr::GET(
    paste0(
      url,
      "api/reference-data",
      "?access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )
  reference_data_content <- httr::content(reference_data_request, as = "text")
  reference_data <- jsonlite::fromJSON(reference_data_content, flatten = TRUE)
  reference_data <- dplyr::filter(reference_data, .data$deleted != TRUE)

    return(reference_data)
}

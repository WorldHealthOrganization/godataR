#' Get user data from Go.Data
#'
#' A function to retrieve a list of all user
#' data and their attributes, across all outbreaks
#' on the instance (since this is a system-level API
#' endpoint). This function relies on the
#' `\users` API endpoint.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns a data frame. Some fields, such as addresses, hospitalization
#' history, and questionnaire fields may require further unnesting. See
#' \code{\link[tidyr]{nest}} for assitance with unnesting.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' users <- get_users(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#' }
#' @export
get_users <- function(url,
                      username,
                      password) {

  users_request <- httr::GET(
    paste0(
      url,
      "api/users",
      "?access_token=",
      godataR::get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )
  users_content <- httr::content(users_request, as = "text")
  users <- jsonlite::fromJSON(users_content, flatten = TRUE)
  users <- dplyr::filter(users, .data$deleted != TRUE)

  return(users)
}

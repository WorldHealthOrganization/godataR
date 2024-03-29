#' Get an access oauth access token for Go.Data
#'
#' A function to retrieve an access token
#' from Go.Data. This is a housekeeping
#' function used in many of the other
#' `godataR` functions.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return string
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' get_access_token(url = url,
#'                  username = username,
#'                  password = password)
#' }
#' @export
get_access_token <- function(url,
                             username,
                             password) {

  response <- httr::POST(
    url = paste0(url, "api/oauth/token?access_token=123"),
    body = list(username = username, password = password),
    encode = "json"
  )

  if (response$status_code == 200) {
    response_json <- httr::content(response, as = "text")
    token <- jsonlite::fromJSON(response_json, flatten = TRUE)$access_token
    return(token)
  } else {
    stop("Error: ", response$status_code)
  }
}

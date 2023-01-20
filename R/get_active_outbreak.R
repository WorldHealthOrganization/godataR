#' Get the currently active outbreak id number
#'
#' A function to retrieve the active outbreak
#' ID number for the provided username. Each
#' Go.Data user can have 1 and only 1 active
#' outbreak at a given time. This is a
#' housekeeping function used in many of the
#' other `godataR` functions.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login.
#'
#' @return
#' Returns a string with the Outbreak ID number of the user's active outbreak.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' active_outbreak_id <- get_active_outbreak(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#' }
get_active_outbreak <- function(url = url,
                                username = username,
                                password = password) {

  godata_url <- httr::GET(
    paste0(
      url,
      "api/users",
      "?access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )

  url_content <- httr::content(godata_url, as = "text")

  users <- jsonlite::fromJSON(url_content, flatten = TRUE)

  active_outbreak <- users$activeOutbreakId[users$email == username]

  return(active_outbreak)
}

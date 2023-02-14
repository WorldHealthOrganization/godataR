#' #' Get teams data from Go.Data
#'
#' A function to retrieve a list of all teams
#' data and their attributes, across all outbreaks
#' on the instance (since this is a system-level API
#' endpoint). This function relies on the
#' `\teams` API endpoint.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns a data frame of teams associated with Go.Data instance.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' teams <- get_teams(url=url,
#'                    username=username,
#'                    password=password)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @export
get_teams <- function(url,
                      username,
                      password) {

  teams_request <- GET(
    paste0(
      url,
      "api/teams",
      "?access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )
  teams_content <- content(teams_request, as = "text")
  teams <- fromJSON(teams_content, flatten = TRUE)
  teams <- filter(teams, .data$deleted != TRUE)

  return(teams)
}

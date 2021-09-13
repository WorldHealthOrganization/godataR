#' Get a list of all teams and their attributes, across all outbreaks on the instance (since this is a system-level API endpoint).
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns data frame of teams associated with Go.Data instance.
#' @export
#' @examples
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' teams <- get_teams(url=url, username=username, password=password)
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @export


get_teams <- function(url=url, username=username, password=password) {
  
  teams <- GET(paste0(url,"api/teams",
                      "?access_token=",godataR::get_access_token(url=url, username=username, password=password))) %>%
    content(as="text") %>%
    fromJSON(flatten=TRUE) %>%
    filter(deleted!=TRUE) 
  
  return(teams)
  
}

#' Get the currently active outbreak id number
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login.
#'
#' @return
#' Returns the Outbreak ID number of the user's active outbreak.
#' @export
#' @examples
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' active_outbreak_id <- get_active_outbreak(url=url, username=username, password=password)
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON

get_active_outbreak <- function(url=url, username=username, password=password) {

  users <- GET(paste0(url,"api/users",
                          "?access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content(as="text") %>%
    fromJSON(flatten=TRUE)

  active.outbreak <- users$activeOutbreakId[users$email==username]

  return(active.outbreak)

}
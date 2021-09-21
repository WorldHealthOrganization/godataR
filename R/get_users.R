#' Get a list of all users and their attributes, across all outbreaks on the instance (since this is a system-level API endpoint).
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns data frame of user accounts associated with Go.Data instance. Some fields, roleIds, will be in list format and may require further un-nesting. See the tidyr::unnest() function.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' users <- get_users(url=url, username=username, password=password)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @export



get_users <- function(url=url, username=username, password=password) {

  users <- GET(paste0(url,"api/users",
                          "?access_token=",godataR::get_access_token(url=url, username=username, password=password))) %>%
    content(as="text") %>%
    fromJSON(flatten=TRUE) %>%
    filter(deleted!=TRUE) %>%
    select(id, firstName, lastName, email, roleIds, lastLoginDate, institutionName, disregardGeographicRestrictions, activeOutbreakId, createdBy, createdAt) 
  return(users)

}

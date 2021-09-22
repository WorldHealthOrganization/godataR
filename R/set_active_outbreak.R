#' Change the currently active outbreak
#'
#' A function to change the user's active outbreak
#' on the Go.Data server. This is a housekeeping
#' function used in many of the other `godataR`
#' functions.
#'
#' Each Go.Data user can have 1 and only 1 active
#' outbreak at a given time.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak to set to active.
#'
#' @return
#' Nothing. A message will indicate whether the active outbreak has been changed in the system.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' set_active_outbreak(url=url,
#'                     username=username,
#'                     password=password,
#'                     outbreak_id=outbreak_id)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON

set_active_outbreak <- function(url=url,
                                username=username,
                                password=password,
                                outbreak_id=outbreak_id) {


  #Get List of Available Outbreaks
  user.details <- GET(paste0(url,"api/users",
                      "?access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content(as="text") %>%
    fromJSON(flatten=TRUE) %>%
    filter(email==username)

  available.outbreaks <- user.details %>%
    pluck("outbreakIds") %>%
    unlist()

  current.active.outbreak <- user.details$activeOutbreakId

  user.id <- user.details$id

  #Is outbreak_id already active?
  if (current.active.outbreak == outbreak_id) {
    text <- paste0("No action taken. ", outbreak_id, " is already active.")
  } else if (!(outbreak_id %in% available.outbreaks)) {
    stop(paste0("No action taken. ",outbreak_id, " not in list of user's available outbreaks. Make sure the id number is correct & that the user has proper access."))
  } else {

    new.data <- list("activeOutbreakId"=outbreak_id)
    patch.active.outbreak <- PATCH(paste0(url,"api/users/",user.id),
                                   add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " ")),
                                   body=new.data,
                                   encode="json")
    text <- paste0("Active outbreak changed! ", outbreak_id, " is now active.")
  }

  message(text)

}

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
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak to set to active.
#'
#' @return
#' Nothing
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

set_active_outbreak <- function(url,
                                username,
                                password,
                                outbreak_id) {


  #Get User ID & Active Outbreak ID
  user_details <- GET(
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
  ) %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    filter(.data$email == username)

  current_active_outbreak <- user_details$activeOutbreakId
  user_id <- user_details$id

  #Get List of Available Outbreak IDs
  available_outbreaks <- get_all_outbreaks(
    url = url,
    username = username,
    password = password
  ) %>%
    select(id) %>%
    unlist()

  if (current_active_outbreak == outbreak_id) { # Is outbreak_id already active?
    text <- paste0(
      "Active outbreak not changed. ",
      outbreak_id,
      " is already active."
    )
  } else if (!(outbreak_id %in% available_outbreaks)) {
    stop(paste0(
      "Active outbreak not changed. ",
      outbreak_id,
      " not in list of user's available outbreaks. Make sure the id number is",
      " correct & that the user has proper access."
    ))
  } else {

    new_data <- list("activeOutbreakId" = outbreak_id)
    patch_active_outbreak <- PATCH(
      paste0(
        url,
        "api/users/",
        user_id
      ),
      add_headers(
        Authorization = paste(
          "Bearer",
          get_access_token(
            url = url,
            username = username,
            password = password
          ),
          sep = " "
        )
      ),
      body = new_data,
      encode = "json"
    )
    text <- paste0("Active outbreak changed! ", outbreak_id, " is now active.")
  }

  message(text)



}

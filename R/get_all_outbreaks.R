#' Get a list of all outbreaks and their attributes
#'
#' A function to retrieve all outbreaks assigned
#' to the provided username. This is a
#' housekeeping function used in many of the
#' other `godataR` functions.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns data frame of outbreaks. The resulting list is filtered by the
#' user's permissions: only outbreaks for which the user has access will be
#' returned.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' outbreaks <- get_all_outbreaks(url=url,
#'                                username=username,
#'                                password=password)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @export
get_all_outbreaks <- function(url,
                              username,
                              password) {

  outbreaks_request <- GET(
    paste0(
      url,
      "api/outbreaks",
      "?access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )

  outbreaks_content <- content(outbreaks_request, as = "text")

  outbreaks <- fromJSON(outbreaks_content, flatten = TRUE)

  outbreaks <- filter(outbreaks, .data$deleted != TRUE)

  outbreaks <- select(
    outbreaks,
    any_of(c("id", "name", "description", "createdBy", "createdAt"))
  )

  return(outbreaks)
}

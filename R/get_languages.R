#' Get lanuages in Go.Data
#'
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#'
#' @return
#' Returns data frame of languages available in Go.Data.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' language <- "english_us"
#'
#' languages <- get_languages(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#' }
get_languages <- function(url,
                          username,
                          password) {

  df_request <- httr::GET(
    paste0(
      url,
      "api/languages/",
      "?access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )

  df_content <- httr::content(df_request, as = "text")

  df <- jsonlite::fromJSON(df_content, flatten = TRUE)

  df <- tibble::as_tibble(df)

  return(df)
}

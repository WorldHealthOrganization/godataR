#' Get a list of variable tokens and their labels for the language you specify,
#' in order to re-code variables in R.
#'
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param language The language ID you are retrieving translation file for,
#' for instance "english_us"
#'
#' @return
#' Returns data frame of language tokens for your language. You will only be
#' able to execute this function if you have access to the language tokens.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' language <- "english_us"
#'
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = language
#' )
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck


get_language_tokens <- function(url,
                                username,
                                password,
                                language) {

  df_request <- GET(
    paste0(
      url,
      "api/languages/",
      language,
      "/language-tokens",
      "?access_token=",
      get_access_token(
        url = url,
        username = username,
        password = password
      )
    )
  )

  df_content <-  content(df_request, as = "text")

  df <- fromJSON(df_content, flatten = TRUE)

  df <- as_tibble(df)

  return(df)

}

#' Get a data model from Go.Data
#'
#' A function to retrieve the data model (data template) for a specific type of object. This is a helper function to make sure that other get_* functions do not return empty results.
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param model The data model to be retrieved.
#'
#' @return
#' Returns a data frame. Data frame will have 0 rows, but its columns and column names can be used as a template.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' case_data_model <- get_data_model(url=url,
#'                                   username=username,
#'                                   password=password,
#'                                   model="case")
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck


get_data_model <- function(url=url,
                           username=username,
                           password=password,
                           model=c("case","contact","followUp","address","age","relationship","event","dateRange","contactOfContact")) {


   data_model <- GET(paste0(url,"api/system-settings/model-definition/?model=",model),
                     add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " "))) %>%
     content(as="text") %>%
     fromJSON(flatten=TRUE) %>%
     nested_cols_to_df() %>%
     as_tibble()

   return(data_model)

}

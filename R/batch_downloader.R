#' Function to manage batch downloads
#'
#' A housekeeping function to do batch downloads.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param api_call_n The API url to get the number of records.
#' @param api_call_get The API url to GET the records.
#' @param batch_size Specifies the number of records to retrieve in each
#' iteration.
#'
#' @return
#' Returns a data frame. Some fields, such as addresses, hospitalization
#' history, and questionnaire fields may require further unnesting. See
#' `\link[tidyr]{nest}` for assitance with unnesting.
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' cases <- get_cases(url=url,
#'                    username=username,
#'                    password=password,
#'                    outbreak_id=outbreak_id)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#'
#'
batch_downloader <- function(url = url,
                             username = username,
                             password = password,
                             api_call_n = api_call_n,
                             api_call_get = api_call_get,
                             batch_size = batch_size) {

  #get total number of records
  df_n <- GET(
    paste0(api_call_n),
    add_headers(
      Authorization = paste("Bearer", get_access_token(
        url = url,
        username = username,
        password = password
      ), sep = " "))) %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    unlist() %>%
    unname()

  #Import records in batches
  df <- tibble()
  batch_size <- batch_size # number of records to import per iteration
  skip <- 0
  message("****************************")

  #Download records in batches, and then append them into a single dataset
  while (skip < df_n) {

    #Progress message
    if (df_n <= batch_size) {
      message(paste0("...downloading records 1 to ", df_n))
    }
    if (df_n > batch_size) {
      message(
        paste0(
          "...downloading records ",
          as.character(skip + 1, scientific = FALSE),
          " to ",
          format(skip + batch_size, scientific = FALSE)
        )
      )
    }

    #fetch the batch of records
    df_i <- GET(
      paste0(
        api_call_get,
        "?filter={%22limit%22:",
        format(batch_size, scientific = FALSE),
        ",%22skip%22:",
        format(skip, scientific = FALSE),
        "}"
      ),
      add_headers(
        Authorization = paste("Bearer", get_access_token(
          url = url,
          username = username,
          password = password),
          sep = " ")
      )
    ) %>%
      content(as = "text") %>%
      fromJSON(flatten = TRUE) %>%
      as_tibble()

    #append the new batch of records to the existing data frame
    df <- df %>%
      bind_rows(df_i)

    #update numbers for the next iteration
    skip <- skip + batch_size
    rm(df_i)
  }
  rm(batch_size, skip, df_n)
  return(df)
}

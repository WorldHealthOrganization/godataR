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
#' cases <- get_cases(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#' }
batch_downloader <- function(url,
                             username,
                             password,
                             api_call_n,
                             api_call_get,
                             batch_size) {

  num_record_request <- httr::GET(
    paste0(api_call_n),
    httr::add_headers(
      Authorization = paste("Bearer", get_access_token(
        url = url,
        username = username,
        password = password
      ), sep = " ")))

  num_record_content <- httr::content(num_record_request, as = "text")

  num_records <- jsonlite::fromJSON(num_record_content, flatten = TRUE)
  num_records <- num_records$count

  #Import records in batches
  df <- tibble::tibble()
  skip <- 0
  message("****************************")

  #Download records in batches, and then append them into a single dataset
  while (skip < num_records) {

    #Progress message
    if (num_records <= batch_size) {
      message(paste0("...downloading records 1 to ", num_records))
    }
    if (num_records > batch_size) {
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
    record_request <- httr::GET(
      paste0(
        api_call_get,
        "?filter={%22limit%22:",
        format(batch_size, scientific = FALSE),
        ",%22skip%22:",
        format(skip, scientific = FALSE),
        "}"
      ),
      httr::add_headers(
        Authorization = paste("Bearer", get_access_token(
          url = url,
          username = username,
          password = password),
          sep = " ")
      )
    )

    record_content <- httr::content(record_request, as = "text")

    records <- jsonlite::fromJSON(record_content, flatten = TRUE)

    records <- tibble::as_tibble(records)

    #append the new batch of records to the existing data frame
    df <- dplyr::bind_rows(df, records)

    #update numbers for the next iteration
    skip <- skip + batch_size
    records <- NULL
  }
  return(df)
}

#' Download clusters from Go.Data (version agnostic)
#'
#' A function to retrieve the cluster data for a
#' specific `outbreak_id`. This function relies
#' on the `/outbreaks/{id}/clusters` API endpoint.
#' Records are imported in iterative batches
#' and then appended together into a single data
#' frame.
#'
#' This function works on all versions of Go.Data.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't
#' forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to
#' download clusters.
#' @param batch_size For large datasets, specify the number of records to
#' retrieve in each iteration.
#'
#' @return
#' Returns data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' clusters <- get_clusters(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#' }
get_clusters <- function(url,
                         username,
                         password,
                         outbreak_id,
                         batch_size = 50000) {

# no /export endpoint for clusters so no need to check version

  outbreak_id_api <- get_active_outbreak(
    url = url,
    username = username,
    password = password
  )
  #Check that outbreak_id is active
  if (outbreak_id != outbreak_id_api) {
    set_active_outbreak(
      url = url,
      username = username,
      password = password,
      outbreak_id = outbreak_id
    )
  }

  #get total number of records
  df_n_request <- httr::GET(
    paste0(url, "api/outbreaks/", outbreak_id, "/clusters/count"),
    httr::add_headers(Authorization = paste("Bearer", get_access_token(
      url = url,
      username = username,
      password = password
    ), sep = " ")))

  df_n_content <- httr::content(df_n_request, as = "text")

  df_n <- jsonlite::fromJSON(df_n_content, flatten = TRUE)
  df_n <- unname(unlist(df_n))

  #Import records in batches
  df <- tibble::tibble()
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
      message(paste0(
        "...downloading records ",
        as.character(skip + 1, scientific = FALSE),
        " to ",
        format(skip + batch_size, scientific = FALSE)
      ))
    }

    #fetch the batch of records
    df_i_request <- httr::GET(
      paste0(
        url,
        "api/outbreaks/",
        outbreak_id,
        "/clusters",
        "/?filter={%22limit%22:",
        format(batch_size, scientific = FALSE),
        ",%22skip%22:",
        format(skip, scientific = FALSE),
        "}"
      ),
      httr::add_headers(Authorization = paste("Bearer", get_access_token(
        url = url,
        username = username,
        password = password
      ), sep = " "))
    )

    df_i_content <- httr::content(df_i_request, as = "text")

    df_i <- jsonlite::fromJSON(df_i_content, flatten = TRUE)

    df_i <- tibble::as_tibble(df_i)

    #append the new batch of records to the existing data frame
    df <- dplyr::bind_rows(df, df_i)

    #update numbers for the next iteration
    skip <- skip + batch_size
    rm(df_i)
  }
  rm(batch_size, skip, df_n)

  return(df)
}

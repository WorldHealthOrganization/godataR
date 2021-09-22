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
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to download clusters.
#' @param batch_size For large datasets, specify the number of records to retrieve in each iteration.
#'
#' @return
#' Returns data frame of clusters.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' clusters <- get_clusters(url=url,
#'                          username=username,
#'                          password=password,
#'                          outbreak_id=outbreak_id)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck

get_clusters <- function(url=url,
                         username=username,
                         password=password,
                         outbreak_id=outbreak_id,
                         batch_size=50000) {

# no /export endpoint for clusters so no need to check version

  #Check that outbreak_id is active
  if (outbreak_id != get_active_outbreak(url=url, username=username, password=password)) {
    set_active_outbreak(url=url, username=username, password=password, outbreak_id=outbreak_id)
  }

  #get total number of records
  df_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/clusters/count"),
              add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " "))) %>%
    content(as="text") %>%
    fromJSON(flatten=TRUE) %>%
    unlist() %>%
    unname()

  #Import records in batches
  df <- tibble()
  batch_size <- batch_size # number of records to import per iteration
  skip <-0
  message("****************************")

  #Download records in batches, and then append them into a single dataset
  while (skip < df_n) {

    #Progress message
    if (df_n <= batch_size) message(paste0("...downloading records 1 to ",df_n))
    if (df_n > batch_size)     message(paste0("...downloading records ", as.character(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))

    #fetch the batch of records
    df.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/clusters",
                       "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"),
                add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " "))) %>%
      content(as='text') %>%
      fromJSON( flatten=TRUE) %>%
      as_tibble()

    #append the new batch of records to the existing data frame
    df <- df %>%
      bind_rows(df.i)

    #update numbers for the next iteration
    skip <- skip + batch_size
    rm(df.i)
  }
  rm(batch_size, skip, df_n)

  return(df)
}

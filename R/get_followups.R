#' Download follow-up of contacts from Go.Data (version 2.38.0 or earlier)
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to download follow-ups.
#' @param batch_size For large datasets, specify the number of records to retrieve in each iteration.
#'
#' @return
#' Returns data frame of follow-ups. Some fields may require further unnesting. See the tidyr::unnest() function.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' followups <- get_followups(url=url, username=username, password=password, outbreak_id=outbreak_id)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
get_followups <- function(url=url, username=username, password=password, outbreak_id=outbreak_id, batch_size=50000) {

  #Check version of Go.Data
  if (check_godata_version(url=url)==TRUE) {
    warning("Your version of Go.Data is 2.38.1 or later. Consider using the function get_followups2().")
  }

  #Check that outbreak_id is active
  #This returns an error if outbreak_id is invalid
  if (outbreak_id != get_active_outbreak(url=url, username=username, password=password)) {
    set_active_outbreak(url=url, username=username, password=password, outbreak_id=outbreak_id)
  }

  #get total number of records
  df_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/follow-ups/filtered-count"),
                    add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " "))) %>%
    content(as="text") %>%
    fromJSON(flatten=TRUE) %>%
    unlist() %>%
    unname()

  #Import data in batches
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
    df.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/follow-ups",
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

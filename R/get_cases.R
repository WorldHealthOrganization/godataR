#' Download cases from Go.Data
#'
#' A function to retrieve the case data for a
#' specific `outbreak_id`.
#'
#' This function works on all versions of Go.Data.There
#' are two methods for downloading the data:
#'
#' `method="batches"` will work on all versions of
#'    Go.Data. This method relies on the GET outbreak/{id}/cases
#'    API endpoint. Cases are then retrieved in batches
#'    based on `batch_size` and appended together into
#'    a final dataset.
#'
#' `method="export"` will only work on Go.Data versions
#'    2.38.1 or newer. This method relies on the GET
#'    outbreak/{id}/cases/export API endpoint. An export
#'    request is submitted to the server, and then when the
#'    export is ready, it will be downloaded. Due to better
#'    performance and more options, `method="export"` will
#'    be the default if you are using Go.Data version 2.28.1
#'    or newer.
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to download cases.
#' @param method The method to download data. `method="export"` is the preferred and default method for Go.Data version 2.38.1 or later. See Details.
#' @param batch_size If `method="batches"`, then `batch_size` specifies the number of records to retrieve in each iteration.
#' @param wait If `method="export"`, then `wait` is the number of seconds to wait in between iterations of checking the status of the export.
#' @param file.type If `method="export"`, then `file.type` determines Whether the resulting data frame should contain nested fields (`file.type="json"`, the default) or an entirely flat data structure (`file.type="csv"`)
#'
#' @return
#' Returns a data frame of cases. Some fields, such as addresses, hospitalization history, and questionnaire fields will require further unnesting. See the \code{\link[tidyr]{unnest}} function.
#' @export
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
get_cases <- function(url=url,
                      username=username,
                      password=password,
                      outbreak_id=outbreak_id,
                      method=c("export","batch"),
                      batch_size=50000,
                      wait=2,
                      file.type=c("json","csv")) {

  #Check that outbreak_id is active
  if (outbreak_id != get_active_outbreak(url=url, username=username, password=password)) {
    set_active_outbreak(url=url, username=username, password=password, outbreak_id=outbreak_id)
  }

  #Set default method
  if (missing(method)) {
    version.check <- check_godata_version(url=url)
    if (version.check) {
      method <- "export"
    } else if (!version.check) {
      method <- "batches"
    }
  }

  #If method = "batches"
  if (method == "batches") {

    #get total number of records
    df_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases/count"),
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
      df.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases",
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


  } else if (method == "export") {

    #Default value of file.type is "json"
    if (missing(file.type)) file.type <- "json"

    #Submit an export request to the system
    if (file.type=="json") {
      request_id <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases/export",
                               "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                               "&access_token=",get_access_token(url=url, username=username, password=password))) %>%
        content() %>%
        pluck("exportLogId")
    } else if (file.type=="csv") {
      request_id <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases/export",
                               "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                               "&type=csv",
                               "&access_token=",get_access_token(url=url, username=username, password=password))) %>%
        content() %>%
        pluck("exportLogId")
    }

    #Check status of request periodcially, until finished
    #function argument 'wait' determines the number of seconds to wait between iterations
    export.request.status <- get_export_status(url=url, username=username, password=password, request_id=request_id)

    while(export.request.status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
      Sys.sleep(wait)
      export.request.status <- GET(paste0(url,"api/export-logs/",request_id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
        content()
      message(paste0("...processed ",export.request.status$processedNo, " of ", export.request.status$totalNo, " records"))
    }

    #Download the export
    message("...beginning download")
    if (file.type=="json") {
      df <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
        content("text", encoding="UTF-8") %>%
        fromJSON(flatten=TRUE)

      # fix one strange variable name
      names(df)[names(df) %in% "_id"] <- "id"
    } else if (file.type=="csv") {
      df <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
        content("text", encoding="UTF-8") %>%
        textConnection() %>%
        read.csv()
      names(df)[names(df) %in% "X_id"] <- "id"
    }

    message("...download complete!")

  }

  return(df)
}

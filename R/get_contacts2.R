#' Download contacts from Go.Data (version 2.38.1 or later)
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#' @param username The email address for your Go.Data login.
#' @param password The password for your Go.Data login
#' @param outbreak_id The id number for the outbreak for which you want to download cases.
#' @param wait The number of seconds to wait in between iterations of checking the status of the download. Default is 5 seconds, but the user can specify a smaller value to speed up the process if the dataset is small.
#' @param include.cases Boolean to include (TRUE) or exclude (FALSE) contacts that became cases
#'
#' @return
#' Returns data frame of cases. Some fields, such as addresses, hospitalization history, and questionnaire fields will require further unnesting. See the tidyr::unnest() function.
#' @export
#' @examples
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' contacts <- get_contacts2(url=url, username=username, password=password, outbreak_id=outbreak_id)
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck

get_contacts2 <- function(url=url, username=username, password=password, outbreak_id=outbreak_id, wait=5, include.cases=FALSE) {

  #Check version of Go.Data
  if (check_godata_version(url=url)==FALSE) {
    stop("Go.Data must be version 2.38.1 or later. Please use the function get_cases() instead.")
  }

  #Check that outbreak_id is active
  if (outbreak_id != get_active_outbreak(url=url, username=username, password=password)) {
    set_active_outbreak(url=url, username=username, password=password, outbreak_id=outbreak_id)
  }

  ##########################
  # Get Contact Data Frame
  ##########################

  #Submit an export request to the system
  export.request <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts/export",
                               "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                               "&access_token=",get_access_token(url=url, username=username, password=password)))

  if (export.request$status_code!=200) {
    stop(paste0('Error code: ', export.request$status_code))
  } else if (export.request$status_code==200) {

    #Get the Request ID
    export.request.id <- export.request %>%
      content() %>%
      pluck("exportLogId")

    #Check status of request periodcially, until finished
    #function argument 'wait' determines the number of seconds to wait between iterations
    message("...preparing download")
    export.request.status <- GET(paste0(url,"api/export-logs/",export.request.id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content() %>%
      pluck("statusStep")
    while(export.request.status != "LNG_STATUS_STEP_EXPORT_FINISHED") {
      Sys.sleep(wait)
      export.request.status <- GET(paste0(url,"api/export-logs/",export.request.id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
        content() %>%
        pluck("statusStep")
      message("...preparing download")
    }

    #Download the export
    message("...beginning download")
    df <- GET(paste0(url,"api/export-logs/",export.request.id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content("text") %>%
      fromJSON(flatten=TRUE)
    message("...download complete!")

    names(df)[names(df) %in% "_id"] <- "id" # fix one strange variable name

    ####################################
    # Get Cases That Used to be Contacts
    ####################################
    # if (include.cases==TRUE) {
    #   message("...downloading cases that used to be contacts")
    #   df.cases <- get_cases2(url=url, username=username, password=password, outbreak_id=outbreak_id, wait=wait) %>%
    #     filter(wasContact==TRUE)
    #   if(nrow(df.cases>0)) df <- df %>% bind_rows(df.cases)
    # }

  }
  return(df)

}

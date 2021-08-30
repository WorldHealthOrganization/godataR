get_cases2 <- function(url=url, username=username, password=password, outbreak_id=outbreak_id, wait=5) {

  #Check version of Go.Data
  gd.version <- get_godata_version(url=url) %>%
    as.data.frame()
  names(gd.version) <- "version"
  gd.version <- gd.version %>%
    select(version) %>%
    separate(version,c("a","b","c")) %>%
    mutate_all(as.numeric)
  if (gd.version$a < 2 | gd.version$b < 38 | gd.version$c < 1) {
      stop("Go.Data must be version 2.38.1 or later. Please use the function get_cases() instead.")
  }

  #Submit an export request to the system
  export.request <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases/export",
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

  }
  names(df)[names(df) %in% "_id"] <- "id" # fix one strange variable name
  return(df)

}

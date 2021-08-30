get_godata_version <- function(url=url) {

  version.request <- GET(paste0(url,"api/system-settings/version"))

  if (version.request$status_code==200) {
    versionJSON <- content(version.request, as="text")
    version <- fromJSON(versionJSON, flatten=TRUE)
    return(version$version)
  } else {
    stop(paste0("Error ",version.request$status_code))
  }

}

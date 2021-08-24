get_godata_version <- function(url=url) {

  version.request <- httr::GET(paste0(url,"api/system-settings/version"))

  if (version.request$status_code==200) {
    versionJSON <- httr::content(version.request, as="text")
    version <- jsonlite::fromJSON(versionJSON, flatten=TRUE)
    message(paste0("Version ", version$version))
    return(version$version)
  } else {
    stop(paste0("Error ",version.request$status_code))
  }

}

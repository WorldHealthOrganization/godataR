get_access_token <- function(url=url, username=username, password=password) {

  response <- httr::POST(url=paste0(url,"api/oauth/token?access_token=123"),
                   body = list(username=username, password=password),
                   encode = "json")

  if (response$status_code==200) {
    responseJSON <- httr::content(response, as="text")
    token <- jsonlite::fromJSON(responseJSON, flatten=TRUE)$access_token
    return(token)
  } else {
    stop(paste0("Error: ",response$status_code))
  }

}

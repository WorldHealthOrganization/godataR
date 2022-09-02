#' Go.Data get credentials for connection string
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function takes Go.Data user credentials as input and returns a one-time
#' connection token to the Go.Data database specified by the user credentials. 
#' The function also returns the internal Go.Data ID for that user and the ID 
#' of their active outbreak. These details are required every time the user 
#' needs to import data from Go.Data to R or export data from R into Go.Data.   
#' 
#' @note 
#' 1. The user must have the required permissions in their Go.Data account in 
#' order to interact with the relevant Go.Data tables in R.  For example, if 
#' the user wants to import or modify lab data, the function "lab manager" must
#' be included as one of their assigned roles in Go.Data.  
#' 
#' 2. It is strongly advised that user credentials be passed to this function
#' securely, e.g. instead of typing the password directly as a function
#' argument, use [getPass::getPass()]. 
#' 
#' 3. The url for Go.Data instances that are off-line are usually composed of a
#' string like "http://localhost:3000/" (last component is the port number). On
#' -line Go.Data instances should have their own web address.
#' 
#' Pre-requisites:
#'  + The user must already have an account in the Go.Data database
#'  + The user must know their Go.Data user credentials 
#'  + The user must know the url for their Go.Data instance
#' @md
#'  
#' @param url URL for Go.Data instance (character string)
#' @param username The email address the user uses to log in to Go.Data
#' @param password The password the user uses to log in to Go.Data 
#' 
#' @returns list: one-time connection token, internal user and outbreak IDs 
#' 
#' @import httr jsonlite
#' 
#' @examples 
#'  \dontrun{
#'  # Enter user credentials and check a token is returned:
#'  print(godata_getids(url = "http://localhost:3000/",
#'                           username = params$username, 
#'                           password = params$pwd))
#'  
#'  # Example use to import case data from Go.Data to R:
#'  cases <- httr::GET(paste0(url,
#'                            "api/outbreaks/",
#'                            godata_getids()$outbreak,
#'                            "/cases?"), 
#'                     add_headers(Authorization = paste("Bearer",
#'                                                       godata_getids()$token,
#'                                                       sep = " ")))
#'  }
#' 
#' @export
godata_getids <- function(url = "http://localhost:3000/", ...) {
  
  # Create the Go.Data connection string with user credentials:
  login = httr::POST(url = paste0(url,"api/users/login"),
                     content_type_json(),
                     body = paste0("{\"email\":\"", 
                                   username, 
                                   "\",\"password\":\"", 
                                   if(is.raw(password) == TRUE){
                                     rawToChar(password)
                                   } else {
                                     password
                                   },
                                   "\",\"token\":null,\"userid\":null}"))
  
  # Fetch token from json format:
  credentials = content(login)
  
  # Save one-time token as token:
  token = credentials$id
  
  # Save Go.Data generated internal user ID:
  userid = credentials$userId
  
  # Fetch user details including which outbreak is active for them:
  userdets = httr::GET(paste0(url, "api/users/", userid),
                       add_headers(Authorization = paste("Bearer", token, sep = " ")))
  
  # Set the active outbreak ID:
  outbreak = content(userdets)$activeOutbreakId
  
  # Return the three items plus url in a list:
  return(list(url = url, token = token, userid = userid, outbreak = outbreak))
  
}
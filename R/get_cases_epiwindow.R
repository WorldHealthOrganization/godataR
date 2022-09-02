#' Import cases from Go.Data notified within a specific date range
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function imports cases from Go.Data with notification dates that fall
#' within a specific date range.  The user first specifies a suitable illness 
#' episode window in days (the number of days beyond which a case still 
#' producing positive samples is likely to have been reinfected).  The episode
#' window to use should be determined with reference to the pathogen 
#' characteristics, as well as national and international case definitions and
#' criteria for defining reinfections. The episode window is then applied to a 
#' date range, and cases are returned if they are within the episode window of
#' the minimum and maximum dates provided.  
#' 
#' An example use case is when a user has laboratory results that they wish to 
#' bulk import to Go.Data, but do not know which Go.Data case IDs the lab 
#' results match with.  Note that the query is designed to return the minimum 
#' amount of patient identifying information necessary to facilitate record 
#' linkage with another data set. This is to ensure a reasonably fast execution 
#' and limit the amount of memory required in R, even when the number of cases 
#' returned is large.
#' 
#' @md
#'  
#' @param url URL (web address) for Go.Data instance
#' @param username User email address used to log in to Go.Data
#' @param password User password used to log in to Go.Data
#' @param epiwindow User defined illness episode window in days (integer)
#' @param mindate Earliest specimen collection date from lab results to match
#' @param maxdate Latest specimen collection date from lab results to match
#' 
#' @return 
#' Returns data.frame of cases to match on, including Go.Data case ID  
#' 
#' @import lubridate
#' @import jsonlite
#' @import httr
#' @import dplyr
#' @import tidyr
#' @import purrr    
#' 
#' @examples 
#' \dontrun{
#' # Get cases notified within 30 days of lab specimen dates 1 - 25 July 2022:
#' cases <- get_cases_epiwindow(url = url, 
#'                              username = username, 
#'                              password = password, 
#'                              mindate = as.Date("2022-07-01"), 
#'                              maxdate = as.Date("2022-07-25"), 
#'                              epiwindow = 30)
#' 
#' # View the result:
#' cases
#' }
#' @export
get_cases_epiwindow <- function(url, 
                                username, 
                                password, 
                                epiwindow, 
                                mindate, 
                                maxdate){
  
  # Check if password needs converting from raw bytes:
  if(is.raw(password)){password = rawToChar(password)}

  ####################################
  # 01. Define date ranges:
  ####################################
  
  # Define and format dates for epiwindow:
  epiwindow = as.numeric(epiwindow)
  mindate = mongify_date(mindate - lubridate::days(epiwindow))
  maxdate = mongify_date(maxdate + lubridate::days(epiwindow))
  
  
  ####################################
  # 02. Create json query:
  ####################################

  # Build the query as an R list object:
  query_list <- list(filter =

                       # Add where clause:
                       list(where =

                       # Filter results by date range:
                       list(dateOfReporting = list(between = c(mindate, maxdate)),

                            # Define format of column names and values:
                            useDbColumns = "true",
                            dontTranslateValues = "true",
                            jsonReplaceUndefinedWithNull = "true"),

                     # Define columns to return:
                     fields = c("id",                # Go.Data system case ID
                                "visualId",          # Go.Data visible case ID
                                "firstName",         # First name of patient
                                "lastName",          # Last name of patient
                                "dob",               # Patient birth date
                                "age.years",         # Patient age in years
                                "dateOfReporting",   # Case notification date
                                "dateOfOnset",       # Case onset date
                                "documents.number")))# Patient document number
  

  # Convert the query to json:
  query_json <- jsonlite::toJSON(x = query_list, 
                                 # Do not indent or space out elements
                                 pretty = FALSE,
                                 # Do not enclose single values in square braces
                                 auto_unbox = TRUE)
  

  ####################################
  # 03. Get active outbreak ID:
  ####################################
  
  # Get the active outbreak ID:
  outbreak_id = get_active_outbreak(url = url, 
                                    username = username, 
                                    password = password)
  
  ####################################
  # 04. Send query to Go.Data:
  ####################################

  # Create the case export request and fetch the export log ID:
  elid = httr::POST(url =

                     # Construct request API URL:
                     paste0(url,
                            "api/outbreaks/",
                            outbreak_id,
                            "/cases/export?access_token=",
                            get_access_token(url = url,
                                             username = username,
                                             password = password)),
                    # Set the content type:
                    httr::content_type_json(),
                    
                    # Add query:
                    body = query_json, 
                    encode = "raw") %>%

    # Fetch content:
    httr::content() %>%
    
    # Extract export log ID from content:
    purrr::pluck("exportLogId")
  
  
  ####################################
  # 05. Fetch query results:
  ####################################
  
  # Now import query results to R using export log ID from the previous step:
  cases = httr::GET(url = 
                      paste0(url, 
                             "api/export-logs/", 
                             elid, 
                             "/download?access_token=", 
                             get_access_token(url = url, 
                                              username = username, 
                                              password = password))) %>% 
    
    # Fetch content of downloaded file:
    httr::content("text", encoding = "UTF-8") %>%
    
    # Convert json to flat data.frame:
    jsonlite::fromJSON(flatten = TRUE) %>% 
    
    # Unnest nested variables:
    tidyr::unnest(cols = documents, names_sep = "_") %>% 
    
    # Convert date columns from mongodb format to R POSIXct:
    dplyr::mutate(across(.cols = c(starts_with("date"), "dob"), 
                         .fns = lubridate::ymd_hms)) %>% 
    
    # Rename columns:
    dplyr::rename_with( ~ gsub(pattern = ".", 
                               replacement = "_", 
                               x = .x, 
                               fixed = TRUE))
  
  
  ####################################
  # 06. Return cases to match on:
  ####################################
  
  # Return data.frame of filtered cases:
  return(cases)
  
}
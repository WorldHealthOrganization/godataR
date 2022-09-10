#' Import cases from Go.Data notified within a specific date range
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function imports cases to R from Go.Data with case notification dates 
#' that fall within a specific date range.  There are three options for 
#' specifying the date ranges:
#'   1. "date range": user specifies a minimum and maximum date
#'   2. "epiwindow": user specifies the last x days to return
#'   3. "epibuffer": adds the epiwindow to the minimum and maximum date
#' 
#' The date that is queried is the date that cases were notified on (a mandatory 
#' field in all case investigation forms in Go.Data called dateOfReporting)
#' 
#' Users can also specify whether to return "all" available columns in the case
#' data export API, or only "identifiers" (a subset of columns comprising only 
#' case identifying data such as first names, last names and dates of birth). 
#' Selecting "identifiers" is recommended for record linkage with another data 
#' set (such as linking lab results to Go.Data case IDs) as this smaller subset 
#' of 9 columns will be returned much faster and use less memory in R for date 
#' ranges that include a lot of cases. 
#' 
#' @details 
#' **Defining the epiwindow:**
#' The user first specifies a suitable illness episode window in days (the 
#' number of days beyond which a case still producing positive samples is likely
#' to have been reinfected).  The episode window to use should be determined 
#' with reference to the pathogen characteristics, as well as national and 
#' international case definitions and criteria for defining reinfections. The 
#' episode window is then applied to a date range, and cases are returned if 
#' they are within the episode window of the minimum and maximum dates provided.
#' 
#' **Defining date element orders**
#' If minimum and maximum dates are supplied to define the date range, the date 
#' element order must be defined (which order is the year, month and day in).  
#' For example, for a mindate of "15/08/2022" and maxdate of "30/08/2022" the 
#' order is first day, then month, then year and the option to select is "dmy".
#' Options are as follows:
#'   + "ymd" select this for year first, then month, then date
#'   + "dmy" select this for day first, then month, then year
#'   + "mdy" select this for month first, then day, then year
#' Note that any separator can be used between the date elements.
#' 
#' **Prerequisites:**
#' Note that this function requires Go.Data user credentials (username, 
#' password and the URL or web address of the Go.Data instance).  Users must 
#' have permission to export case data within Go.Data.  By default, cases will
#' be returned for the user's active outbreak.  If the user wishes to query a 
#' different outbreak, the Go.Data outbreak ID for the outbreak of interest 
#' should be supplied.  To obtain the IDs of non-active outbreaks, use 
#' `godataR::get_all_outbreaks()` before running this function.
#'   
#' @md
#'  
#' @param url URL (web address) for Go.Data instance
#' @param username User email address used to log in to Go.Data
#' @param password User password used to log in to Go.Data
#' @param outbreak Outbreak to use; "active" (default) or other outbreak ID
#' @param cols2return Set of columns to return; either "identifiers" or "all"
#' @param datequery Date query method; "date range", "epiwindow" or "epibuffer"
#' @param daterangeformat Min & max date element order; "ymd", "dmy" or "mdy"  
#' @param epiwindow User-defined illness episode window in days (integer)
#' @param mindate Minimum case notification date for date range
#' @param maxdate Maximum case notification date for date range
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
#' # Get cases from the active outbreak notified within the last 30 days:
#' cases <- get_cases_epiwindow(url = url, 
#'                              username = username, 
#'                              password = password,
#'                              cols2return = "all",
#'                              datequery = "epiwindow, 
#'                              epiwindow = 30)
#' 
#' # Get cases from 01 August to 25 September 2022 with a 30-day buffer:
#' cases <- get_cases_epiwindow(url = url, 
#'                              username = username, 
#'                              password = rawToChar(password), 
#'                              cols2return = "identifiers", 
#'                              datequery = "epibuffer", 
#'                              epiwindow = 30, 
#'                              daterangeformat = "dmy", 
#'                              mindate = "01/08/2022", 
#'                              maxdate = "25/09/2022")
#' 
#' # View the result:
#' cases
#' }
#' @export
get_cases_epiwindow <- function(url, 
                                username, 
                                password,
                                outbreak = "active",
                                cols2return = c("identifiers", 
                                                "all"),
                                datequery = c("date range", 
                                              "epiwindow", 
                                              "epibuffer"),
                                daterangeformat = c("ymd", 
                                                    "dmy", 
                                                    "mdy"),
                                epiwindow, 
                                mindate = NULL, 
                                maxdate = NULL){
  
  
  # Check if requisite arguments are supplied, exit with an error if not:
  if(datequery == "date range" &
     (is.null(mindate) 
      | is.null(maxdate) 
      | is.null(daterangeformat))){
    
    stop("Some arguments required to perform the date query are missing. 
         For 'date range', specify mindate, maxdate and daterangeformat.")
    
  } else if(datequery == "epiwindow" &
            (is.null(epiwindow))){
    
    stop("Some arguments required to perform the date query are missing.
         For 'epiwindow', specify the epiwindow in days.")
    
  } else if(datequery == "epibuffer" & 
            (is.null(mindate) 
             | is.null(maxdate) 
             | is.null(daterangeformat)
             | is.null(epiwindow))){
    
    stop("Some arguments required to perform the date query are missing.
         For 'epibuffer', specify mindate, maxdate, daterangeformat & epiwindow")
    
  }
  
  # Check if password needs converting from raw bytes:
  if(is.raw(password)){password = rawToChar(password)}

  ####################################
  # 01. Define date ranges:
  ####################################
  
  # Check that epiwindow is in the correct format:
  epiwindow = as.numeric(epiwindow)
  
  # Define date ranges:
  if(datequery == "date range"){
    
    mindate = mongify_date(mindate, dateformat = daterangeformat)
    maxdate = mongify_date(maxdate, dateformat = daterangeformat)
     
  } else if(datequery == "epiwindow"){
    
    # Subtract the epiwindow from today's date to get the minimum date:
    mindate = mongify_date(Sys.Date() - lubridate::days(epiwindow))
    
    # Use today's date/time (right now) as the maximum date:
    maxdate = mongify_date(Sys.time())
    
  } else if(datequery == "epibuffer"){
    
    # First convert the supplied min and max dates to date format:
    mindatef = lubridate::parse_date_time(x = mindate, orders = daterangeformat)
    maxdatef = lubridate::parse_date_time(x = maxdate, orders = daterangeformat)
    
    # Next calculate the min and max dates applying the epiwindow buffer:
    mindate = mongify_date(mindatef - lubridate::days(epiwindow))
    maxdate = mongify_date(maxdatef + lubridate::days(epiwindow))
    
  }
  
  
  ####################################
  # 02. Create json query:
  ####################################

  # Build the query with or without defining columns to return:
  if(cols2return == "identifiers"){
    
    # Build the query as an R list object:
    query_list <- list(filter =
                         
                         # Add where clause:
                         list(where =
                                
                                # Filter results by date range:
                                list(dateOfReporting = list(between = 
                                                              c(mindate, 
                                                                maxdate)),
                                     
                                     # Define format of column names and values:
                                     useDbColumns = "true",
                                     dontTranslateValues = "true",
                                     jsonReplaceUndefinedWithNull = "true"),
                              
                              # Define columns to return:
                              fields = c("id",                # System case ID
                                         "visualId",          # Visible case ID
                                         "firstName",         # Case first name
                                         "lastName",          # Case last name
                                         "dob",               # Case birth date
                                         "age.years",         # Case age (years)
                                         "dateOfReporting",   # Case report date
                                         "dateOfOnset",       # Case onset date
                                         "documents.number")))# Case document ID
    
  } else if(cols2return == "all"){
    
    # Build the query as an R list object:
    query_list <- list(filter =
                         
                         # Add where clause:
                         list(where =
                                
                                # Filter results by date range:
                                list(dateOfReporting = list(between = 
                                                              c(mindate, 
                                                                maxdate)),
                                     
                                     # Define format of column names and values:
                                     useDbColumns = "true",
                                     dontTranslateValues = "true",
                                     jsonReplaceUndefinedWithNull = "true")))
    
  }
  

  # Convert the query to json:
  query_json <- jsonlite::toJSON(x = query_list, 
                                 # Do not indent or space out elements
                                 pretty = FALSE,
                                 # Do not enclose single values in square braces
                                 auto_unbox = TRUE)
  

  ####################################
  # 03. Get active outbreak ID:
  ####################################
  
  if(outbreak == "active"){
    
    # Get the active outbreak ID:
    outbreak_id = get_active_outbreak(url = url, 
                                      username = username, 
                                      password = password)
    
  } else {
    
    # Set outbreak ID to that supplied by user:
    outbreak_id = outbreak
    
  }
  
  
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
  # 05. Wait for download to compile:
  ####################################
  
  # Check status of request periodically, until finished
  er_status = httr::GET(paste0(url,
                               "api/export-logs/",
                               elid,
                               "?access_token=",
                               get_access_token(url = url,
                                                username = username,
                                                password = password))) %>% 
    # Extract content:
    content() 
  
  # Subset content to extract necessary messages:
  er_status = er_status[c("statusStep",
                          "totalNo",
                          "processedNo")]
  
  # Set waiting time to allow download to complete:
  while(er_status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
    
    # Wait for request to complete:
    Sys.sleep(2)
    
    # Get export request status again:
    er_status = hhtr::GET(paste0(url,
                                 "api/export-logs/",
                                 elid,
                                 "?access_token=",
                                 get_access_token(url = url,
                                                  username = username,
                                                  password = password))) %>% 
      # Extract content again:
      content()
    
    # Set user progress message:
    message(paste0("...processed ",
                   er_status$processedNo,
                   " of ", 
                   er_status$totalNo, " records"))
    
    }
  
  ####################################
  # 06. Fetch query results:
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
  # 07. Return cases to match on:
  ####################################
  
  # Return data.frame of filtered cases:
  return(cases)
  
}
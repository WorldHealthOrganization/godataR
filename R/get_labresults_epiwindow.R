#' Import lab results from Go.Data within a specific date range
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function imports lab results to R from Go.Data with sample dates that 
#' fall within a specific date range.  There are three options for 
#' specifying the date ranges:
#'   1. "date range": user specifies a minimum and maximum date
#'   2. "epiwindow": user specifies the last x days to return
#'   3. "sample dates": user provides a vector of sample dates to search for
#' 
#' Users can also specify whether to return "all" available columns in the lab 
#' results export API, or only "update" (a subset of columns needed to 
#' match lab results to be updated with existing lab records in Go.Data).
#' The second option is recommended if the user wishes to update existing lab 
#' results and will include patient identifier columns for record linkage.  
#' Note that some of the columns to match on are derived from case data.   
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
#' If minimum and maximum dates or a vector of sample dates are supplied to 
#' define the date range, the date element order must be defined (which order 
#' is the year, month and day in). For example, for a mindate of "15/08/2022" 
#' and maxdate of "30/08/2022" the order is first day, then month, then year 
#' and the option to select is "dmy". Options are as follows:
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
#' @param cols2return Set of columns to return; either "update" or "all"
#' @param datequery Date query method; "date range", "epiwindow", "sample dates"
#' @param daterangeformat Min & max date element order; "ymd", "dmy" or "mdy"  
#' @param epiwindow User-defined illness episode window in days (integer)
#' @param mindate Minimum date for date range
#' @param maxdate Maximum date for date range
#' @param sampledates Vector of specimen dates to search for
#' 
#' @return 
#' Returns data.frame of lab results, including Go.Data lab record IDs  
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
#' # Get lab results for samples collected within the last 30 days:
#' labres <- get_labresults_epiwindow(url = url, 
#'                                    username = username, 
#'                                    password = password,
#'                                    cols2return = "all",
#'                                    datequery = "epiwindow", 
#'                                    epiwindow = 30)
#' 
#' # Create vector of sample dates to search on:
#' mysampledates <- c("2022-07-14", "2022-08-16", "2022-08-17")
#' 
#' # Get lab records matching the vector of sample dates:
#' labres <- get_labresults_epiwindow(url = url, 
#'                                    username = username, 
#'                                    password = password, 
#'                                    cols2return = "update", 
#'                                    datequery = "sampledates", 
#'                                    daterangeformat = "ymd",
#'                                    sampledates = mysampledates)
#' 
#' # View the result:
#' labres
#' }
#' @export
get_labresults_epiwindow <- function(url, 
                                     username, 
                                     password,
                                     outbreak = "active",
                                     cols2return = c("update", "all"),
                                     datequery = c("date range", 
                                                   "epiwindow", 
                                                   "sampledates"),
                                     epiwindow = NULL,
                                     daterangeformat = c("ymd", "dmy", "mdy"),
                                     mindate = NULL, 
                                     maxdate = NULL, 
                                     sampledates = NULL){
  
  
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
    
  } else if(datequery == "sampledates" & 
            (is.null(daterangeformat) 
             | is.null(sampledates))){
    
    stop("Some arguments required to perform the date query are missing.
         For 'sampledates', specify daterangeformat and provide sampledates")
    
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
    
  } else if(datequery == "sampledates"){
    
    # Convert the vector of sample dates to Mongo DB date-time format:
    sampledates = mongify_date(sampledates, dateformat = daterangeformat)

  }
  
  
  ####################################
  # 02. Create json query for lab:
  ####################################
  
  # Build the query between two dates with restricted fields to return:
  if(cols2return == "update" & datequery != "sampledates"){
    
    # Build the query as an R list object:
    query_list_l <- list(filter =
                         
                         # Add where clause:
                         list(where =
                                
                                # Filter results by date range:
                                list(dateSampleTaken = list(between = 
                                                              c(mindate, 
                                                                maxdate)),
                                     
                                     # Define format of column names and values:
                                     useDbColumns = "true",
                                     dontTranslateValues = "true",
                                     jsonReplaceUndefinedWithNull = "true"),
                              
                              # Define columns to return:
                              fields = c("id",                # Lab record ID
                                         "updatedAt",         # System timestamp
                                         "person.visualId",   # Visible case ID
                                         "labName",           # Name of lab
                                         "sampleIdentifier",  # Lab sample ID
                                         "dateSampleTaken",   # Lab sample date
                                         "sequence.hasSequence")))# Boolean
  
    
    # Build the query to search for a vector of dates with restricted fields:  
  } else if(cols2return == "update" & datequery == "sampledates"){
    
    # Build the query as an R list object:
    query_list_l <- list(filter =
                         
                         # Add where clause:
                         list(where =
                                
                                # Filter results by date range:
                                list(dateSampleTaken = list('$in' = sampledates),
                                     
                                     # Define format of column names and values:
                                     useDbColumns = "true",
                                     dontTranslateValues = "true",
                                     jsonReplaceUndefinedWithNull = "true"),
                              
                              # Define columns to return:
                              fields = c("id",                # Lab record ID
                                         "updatedAt",         # System timestamp
                                         "person.visualId",   # Visible case ID
                                         "labName",           # Name of lab
                                         "sampleIdentifier",  # Lab sample ID
                                         "dateSampleTaken",   # Lab sample date
                                         "sequence.hasSequence")))# Boolean
    
    # Build the query to search between two dates and return all fields:
  } else if(cols2return == "all" & datequery != "sampledates"){
    
    # Build the query as an R list object:
    query_list_l <- list(filter =
                         
                         # Add where clause:
                         list(where =
                                
                                # Filter results by date range:
                                list(dateSampleTaken = list(between = 
                                                              c(mindate, 
                                                                maxdate)),
                                     
                                     # Define format of column names and values:
                                     useDbColumns = "true",
                                     dontTranslateValues = "true",
                                     jsonReplaceUndefinedWithNull = "true")))
    
    # Build the query to search for a vector of dates and return all fields:
  } else if(cols2return == "all" & datequery == "sampledates"){
    
    # Build the query as an R list object:
    query_list_l <- list(filter =
                         
                         # Add where clause:
                         list(where =
                                
                                # Filter results by date range:
                                list(dateSampleTaken = list('$in' = sampledates),
                                     
                                     # Define format of column names and values:
                                     useDbColumns = "true",
                                     dontTranslateValues = "true",
                                     jsonReplaceUndefinedWithNull = "true")))
    
  }
  
  # Convert the lab query to json:
  query_json_l <- jsonlite::toJSON(x = query_list_l, 
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
  
  # Create the lab results export request and fetch the export log ID:
  elid = httr::POST(url =
                      
                      # Construct request API URL:
                      paste0(url,
                             "api/outbreaks/",
                             outbreak_id,
                             "/lab-results/export?access_token=",
                             get_access_token(url = url,
                                              username = username,
                                              password = password)),
                    # Set the content type:
                    httr::content_type_json(),
                    
                    # Add query:
                    body = query_json_l, 
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
  labresults = httr::GET(url = 
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
    
    # Convert date columns from mongodb format to R POSIXct:
    dplyr::mutate(across(.cols = c(starts_with("date"), "updatedAt"), 
                         .fns = lubridate::ymd_hms)) %>% 
    
    # Remove language token from lab name for readability:
    dplyr::mutate(labName = gsub(pattern = "LNG_REFERENCE_DATA_CATEGORY_LAB_NAME_", 
                                 replacement = "", 
                                 x = labName)) %>% 
    
    # Rename columns:
    dplyr::rename_with( ~ gsub(pattern = ".", 
                               replacement = "_", 
                               x = .x, 
                               fixed = TRUE)) %>% 
    
    # Rename columns for export:
    dplyr::rename(godata_system_lid = '_id', 
                  godata_labupdatedat = updatedAt,
                  godata_sampledate = dateSampleTaken,
                  sample_id = sampleIdentifier, 
                  sequenced = sequence_hasSequence, 
                  godata_caseid = person_visualId) %>% 
    
    # Reorder columns with compulsory ones first:
    dplyr::relocate(godata_systemlid, 
                    godata_labupdatedat, 
                    godata_caseid, 
                    godata_sampledate,
                    sample_id,
                    sequenced, 
                    labName)
  
  
  #########################################
  # 07. Retrieve Go.Data case identifiers:
  #########################################
  
  # Build the query to retrieve case identifiers with Go.Data case IDs 
  # from the previous step (labresults$godata_caseid):
  query_list_c <- list(filter =
                         
                         # Add where clause:
                         list(where =
                                
                                # Filter results by date range:
                                list(visualId = 
                                       list('$in' = labresults$godata_caseid),
                                     
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
  
  ############################################## 
  # Convert the Go.Data case ID query to JSON:
  query_json_c <- jsonlite::toJSON(x = query_list_c, 
                                   # Do not indent or space out elements
                                   pretty = FALSE,
                                   # Do not enclose single values in square braces
                                   auto_unbox = TRUE)
  
  ############################################## 
  # Send the Go.Data case ID query to Go.Data and fetch the export log ID:
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
                    body = query_json_c, 
                    encode = "raw") %>%
    
    # Fetch content:
    httr::content() %>%
    
    # Extract export log ID from content:
    purrr::pluck("exportLogId")
  
  ############################################## 
  # Check the status of the case download periodically:
  
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
  
  ############################################## 
  # Retrieve the case data:
  
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
                               fixed = TRUE)) %>% 
    
    # Rename first column:
    dplyr::rename(godata_system_cid = '_id', 
                  godata_caseid = visualId)
  
  
  ####################################
  # 08. Merge lab and case data:
  ####################################
  
  # Merge lab and case data with a left join:
  labout = dplyr::left_join(x = labresults, 
                            y = cases, 
                            by = "godata_caseid")

  
  
  ####################################
  # 09. Return cases to match on:
  ####################################
  
  # Return data.frame of filtered lab results with case identifiers:
  return(labout)
  
}
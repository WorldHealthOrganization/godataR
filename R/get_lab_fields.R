#' Get lookup table of Go.Data lab result fields 
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function creates a look-up table of lab result fields (both core and 
#' user-designed questionnaire) and their hard-coded values, where relevant.
#' The fields and values are extracted for the active outbreak of a user's 
#' Go.Data instance.  Values will include user-defined values that were added 
#' to the reference sets in Go.Data.  The fields and values can then be mapped
#' to a laboratory data file, in preparation for importing to Go.Data.
#' 
#' @md
#'  
#' @param url URL (web address) for Go.Data instance
#' @param username User email address used to log in to Go.Data
#' @param password User password used to log in to Go.Data
#' 
#' @return 
#' Returns a list of lab fields including values to map where relevant  
#' 
#' @import httr
#' @import jsonlite
#' @import urltools
#' @import dplyr
#' @import tidyr
#' @import stringr
#' 
#' @examples 
#' \dontrun{
#' # Get lab fields from a specific Go.Data instance:
#' labvars <- get_lab_fields(url = url, 
#'                           username = username, 
#'                           password = password)
#' 
#' # View the result:
#' labvars
#' }
#' @export
get_lab_fields <- function(url = url, 
                           username = username, 
                           password = password){
  
  
  ####################################
  # 01. Set user credentials:
  ####################################
  
  # Check if password needs converting from raw bytes:
  if(is.raw(password)){password = rawToChar(password)}
  
  # Get the active outbreak ID:
  outbreak_id = get_active_outbreak(url = url, 
                                    username = username, 
                                    password = password)
  
  #####################################################
  # 02. Get user-defined lab questionnaire fields:
  #####################################################
  
  # Build the lab questionnaire query as a list:
  query_list_q <- list(where = 
                       
                       # Set value formats:
                       list(useDbColumns = "true", 
                            dontTranslateValues = "true", 
                            jsonReplaceUndefinedWithNull = "true"), 
                     
                     # Bring back only lab results questions:
                     fields = "labResultsTemplate")
  
  # Convert the lab questionnaire query to json:
  query_json_q <- jsonlite::toJSON(x = query_list_q, 
                                 # Do not indent or space out elements
                                 pretty = FALSE,
                                 # Do not enclose single values in square braces
                                 auto_unbox = TRUE)
  
  # Encode the lab questionnaire json query:
  query_encoded_q = urltools::url_encode(query_json_q)
  
  # Create the case export request and fetch the export log ID:
  labfields = httr::GET(url = 
                     
                     # Construct request API URL:
                     paste0(url, 
                            "api/outbreaks/", 
                            outbreak_id, 
                            "?filter=", 
                            query_encoded_q,
                            "&access_token=", 
                            get_access_token(url = url, 
                                             username = username, 
                                             password = password))) %>% 
    
    # Fetch content:
    httr::content("text", encoding = "UTF-8") %>%
    
    # Convert json to flat data.frame:
    jsonlite::fromJSON(flatten = TRUE)
  
  # Unpack data.frame of lab questionnaire fields from list:
  labfieldstab = labfields[["labResultsTemplate"]] %>% 
    
    # Remove language token from answerType column for easier filtering:
    mutate(answerType = gsub("LNG_REFERENCE_DATA_CATEGORY_QUESTION_ANSWER_TYPE_", 
                             "", 
                             answerType, 
                             ignore.case = TRUE)) %>% 
    
    # Remove markup rows:
    filter(answerType != "MARKUP") 
  
  # Un-nest value columns:
  labvaluecols = labfieldstab %>% 
    
    # Separate out fields with hard-coded answers to map:
    filter(answerType %in% c("SINGLE_ANSWER", "MULTIPLE_ANSWER")) %>% 
    
    # Unnest nested variables:
    tidyr::unnest(col = "answers", names_sep = "_") %>% 
    
    # Subset to only necessary columns:
    dplyr::select(variable, answerType, answers_value)
  
  # Reformat free text and numeric columns:
  labfreetext = labfieldstab %>% 
    
    # Separate out fields that do not have hard-coded answers:
    filter(answerType %in% c("FREE_TEXT", "NUMERIC")) %>% 
    
    # Separate out required columns:
    dplyr::select(variable, answerType)
    
  # Merge value and free text lookup tables:
  labquest = dplyr::bind_rows(labvaluecols, labfreetext) %>% 
    
    # Clean up answer type labels:
    mutate(answerType = recode(.x = answerType, 
                               FREE_TEXT = "string", 
                               NUMERIC = "numeric", 
                               SINGLE_ANSWER = "value map", 
                               MULTIPLE_ANSWERS = "value map", 
                               DATE_TIME = "date", 
                               BOOLEAN = "boolean", 
                               FILE_UPLOAD = "file")) %>% 
    
    # Create variable and value labels:
    mutate(variable_label = toupper(variable), 
           answer_label = toupper(answers_value)) %>% 
    
    # Clean up names:
    rename(variable_code = variable,
           answer_type = answerType, 
           answer_code = answers_value) %>% 
    
    # Reorder columns:
    relocate(variable_label, .before = answer_type)
    

  ####################################
  # 03. Get core lab result fields:
  ####################################
  
  # List of core lab fields that require value mapping:
  labfieldsval = c("personType", 
                   "labName", 
                   "sampleType", 
                   "testType", 
                   "result", 
                   "status", 
                   "sequence.labId", 
                   "sequence.resultId")
  
  # Retrieve list of core lab fields:
  labcore = httr::GET(url = 
                        paste0(url, 
                               "api/system-settings/model-definition?model=",
                               "labResult", 
                               "&access_token=",
                               get_access_token(url = url, 
                                                username = username, 
                                                password = password))) %>% 
    
    # Fetch content:
    httr::content("text", encoding = "UTF-8") %>%
    
    # Convert json to flat file:
    jsonlite::fromJSON(flatten = FALSE) %>% 
    
    # Convert list to data.frame:
    data.frame() %>% 
    
    # Transpose:
    tidyr::pivot_longer(cols = tidyr::everything(), 
                        names_to = "variable", 
                        values_to = "answerType") %>% 
    
    # Tag questions that need value mapping:
    mutate(answerType = if_else(variable %in% labfieldsval, 
                                "value map", 
                                answerType)) %>% 
    
    # Add question labels:
    mutate(categoryId = case_when(variable == "personType" ~ "PERSON TYPE", 
                                  variable == "labName" ~ "LAB NAME", 
                                  variable == "sampleType" ~ "TYPE OF SAMPLE", 
                                  variable == "testType" ~ "TYPE OF LAB TEST", 
                                  variable == "result" ~ "LAB TEST RESULT", 
                                  variable == "status" ~ "LAB TEST RESULT STATUS", 
                                  variable == "sequence.hasSequence" ~ "HAS SEQUENCE",
                                  variable == "sequence.noSequenceReason" ~ "NO SEQUENCE REASON", 
                                  variable == "sequence.dateSampleSent" ~ "DATE SENT FOR SEQUENCING",
                                  variable == "sequence.dateResult" ~ "DATE SEQUENCE REPORTED",
                                  variable == "sequence.labId" ~ "LAB SEQUENCE LABORATORY", 
                                  variable == "sequence.resultId" ~ "LAB SEQUENCE RESULT"))
  
  # Get reference values for lab fields:
  labrefvals = get_reference_data(url = url,
                                  username = username, 
                                  password = password) %>% 
    
    # Filter for lab questions:
    dplyr::filter(stringr::str_detect(string = categoryId, 
                                      pattern = "LAB|SAMPLE|PERSON_TYPE") 
                  & active == TRUE) %>% 
    
    # Remove language token to facilitate matching to lab variables:
    mutate(across(.cols = c(categoryId, value), 
                  ~ gsub(pattern = "LNG_REFERENCE_DATA_CATEGORY_",
                         replacement = "", 
                         x = .x)))  %>% 
    
    # Remove question label from answer (value):
    mutate(value = stringr::str_remove_all(string = value, 
                                           pattern = paste0(categoryId, "_"))) %>%
    
    # Remove underscore:
    mutate(across(.cols = c(categoryId, value), 
                  ~ gsub(pattern = "_", 
                         replacement = " ", 
                         x = .x))) %>% 
    
    # Limit to the three required columns to join on:
    dplyr::select(categoryId, value, code)
  
  # Merge labcore with reference values:
  labcorefull = dplyr::full_join(x = labcore, 
                                  y = labrefvals, 
                                  by = "categoryId") %>% 
    
    # Remove unnecessary values:
    filter(!variable %in% c("createdAt", 
                            "createdBy", 
                            "updatedBy", 
                            "createdOn", 
                            "deleted", 
                            "deletedAt", 
                            "deletedByParent", 
                            "questionnaireAnswers", 
                            "outbreakId")) %>% 
    
    # Label ID field for clarity:
    mutate(categoryId = case_when(!is.na(categoryId) ~ categoryId, 
                                  is.na(categoryId) ~ stringr::str_replace_all(string = variable, 
                                                                               pattern = "([[:upper:]])", 
                                                                               replacement = " \\1"), 
                                  variable == "id" ~ "LAB RECORD ID", 
                                  variable == "sequence.hasSequence" ~ "SAMPLE SEQUENCED", 
                                  variable == "sequence.noSequenceReason" ~ "REASON NOT SEQUENCED", 
                                  variable == "sequence.dateSampleSent" ~ "DATE SENT FOR SEQUENCING", 
                                  variable == "sequence.labId" ~ "NAME OF SEQUENCING LAB", 
                                  variable == "sequence.dateResult" ~ "DATE OF SEQUENCE RESULT", 
                                  variable == "sequence.resultId" ~ "SEQUENCE VARIANT TYPE")) %>% 
    
    # Make all categoryId values upper case for display in the app:
    mutate(categoryId = toupper(categoryId)) %>% 
    
    # Add missing answer codes from answer labels:
    mutate(code = if_else(condition = (is.na(code) | code == "") & !is.na(value), 
                          true = value,
                          false = code)) %>% 
    
    # Rename and relocate variables:
    relocate(variable_code = variable, 
             variable_label = categoryId, 
             answer_type = answerType, 
             answer_code = code, 
             answer_label = value)
  
  
  #############################################
  # 04. Create patient identifier fields table:
  #############################################
  
  # Create patient identifiers table:
  pids = tibble(variable_code = c("firstName", "lastName", "birthDate", "documents.number"), 
                variable_label = c("FIRST NAME", "SURNAME", "DATE OF BIRTH", "DOCUMENT ID"), 
                answer_type = c("string", "string", "date", "string"), 
                answer_code = NA_character_, 
                answer_label = NA_character_)
  
  
  #############################################
  # 05. Create final lab fields lookup table:
  #############################################
  
  # Merge pid, lab core and lab questionnaire tables:
  labvars_lookup = dplyr::bind_rows("pid" = pids, 
                                    "core" = labcorefull, 
                                    "questionnaire" = labquest, 
                                    .id = "category") %>% 
    
    # Update label for lab record ID for clarity:
    mutate(variable_label = recode(variable_label, ID = "LAB RECORD ID")) 
  
  
  # Return complete lookup table:
  return(labvars_lookup)  

}
#' Link lab results to Go.Data cases 
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function will perform exact or fuzzy matching to match laboratory 
#' records to Go.Data cases, based on common identifiers that are present
#' in both data sets.
#' 
#' Exact matching is the default if the selected identifier column is document 
#' ID; alternatively if fuzzy matching is selected, a Damerau-Levenshtein 
#' distance of 1 will be used.
#' 
#' For identifiers that include first names and surnames, if fuzzy matching is 
#' selected (the default) then the names will be converted to soundex codes 
#' and matched on this basis.  Note that soundex codes are less discriminatory
#' with longer names; it is therefore advisable to include both first name, 
#' surname and date of birth or age if possible, to increase the accuracy of 
#' the matches.  
#' 
#' For date of birth, a Damerau-Levenshtein distance of 1 is used (this allows 
#' for a maximum of one transposition, e.g. 21 written as 12).
#' 
#' For age, only exact matching is performed as there are only two digits.
#' 
#' In order to return a match, all the selected identifiers must match under 
#' the selected criteria. For example: if first name, surname and birth date 
#' were selected with fuzzy matching, matched cases would have to have the same 
#' soundex codes for first name AND surname AND the same date of birth 
#' (allowing for a maximum of one transposition).
#' 
#' @md
#'  
#' @param labdata data.frame of laboratory records to match to Go.Data cases
#' @param casedata data.frame of case data imported from Go.Data
#' @param matchcols one of "names & dob", "names & age", "names" or "doc ID".
#' @param matchmethod one of "fuzzy" or "exact"
#' 
#' @return updated lab data with Go.data case IDs for identified matches.
#' 
#' @import lubridate
#' @import dplyr
#' @import dtplyr
#' @import data.table
#' @import stringdist
#' 
#' @examples 
#' # Create a character vector of dates:
#' x <- c("2022-07-15", "2021-08-09", NA, "2022-08-03")
#' 
#' # Get date range:
#' daterange <- get_date_range(dates = x)
#' 
#' # View the result:
#' daterange
#' @export
match_lab2cases <- function(labdata, 
                            casedata,
                            matchcols = c("names & dob", 
                                          "names & age", 
                                          "names", 
                                          "doc ID"), 
                            matchmethod = c("fuzzy", 
                                            "exact")){
  
  ###########################
  # 1. Format match columns:
  ###########################
  
  if(matchcols == "names & dob"){
    
    # Check that birthdates in lab data are formatted as dates, convert if not:
    labdata = labdata %>% 
      
      mutate(dob = if_else(condition = any(class(dates) %in% c("Date", "POSIXt")), 
                           true = dob, 
                           false = lubridate::parse_date_time(x = birthdate, 
                                                              orders = c("ymd", 
                                                                         "dmy", 
                                                                         "mdy"))))
  
  }
  
  if(matchcols == "names & age"){
    
    # Make sure age in lab data is integer:
    labdata = labdata %>% 
      
      mutate(age_years = as.integer(age_years))
    
  }
  
  if(matchcols == "doc ID"){
    
    # Convert lab data to lower case and strip white spaces:
    labdata = labdata %>% 
      
      mutate(documents_number = tolower(gsub(pattern = " ", 
                                  replacement = "", 
                                  x = documents_number)))
    
    # Convert case data to lower case and strip white spaces:
    casedata = casedata %>% 
      
      mutate(documents_number = tolower(gsub(pattern = " ", 
                                             replacement = "", 
                                             x = documents_number)))
    
  }
  
  
  ###########################
  # 2. Perform matching:
  ###########################
  
  # Convert case and lab data to data.table 
  # to improve speed and memory consumption of matching process
  
  # Convert case data to data.table:
  casedata = casedata %>% 
    as.data.table()
  
  # Convert lab data tp data.table:
  labdata = labdata %>% 
    as.data.table()
  
  
  if(matchmethod == "exact" & matchcols == "names & dob"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check if any first names match exactly:
    labdata[, dscore_fn := match(x = firstName, table = casedata$firstName)]
    
    # Check if any surnames match exactly:
    labdata[, dscore_sn := match(x = lastName, table = casedata$lastName)]
    
    # Check if any dates of birth match exactly:
    labdata[, dscore_dob := match(x = dob, table = casedata$dob)]
    
    # Return case match index if all three columns match, else NA:
    labdata[, casematch := fifelse(test = !is.na(any(dscore_fn, 
                                                     dscore_sn, 
                                                     dscore_dob)) & 
                                     var(c(dscore_fn, 
                                           dscore_sn, 
                                           dscore_dob)) == 0, 
                                   yes = dscore_fn, 
                                   no = NA_real_), 
            by = 1:nrow(labdata)]
    
    # Add the Go.Data visualId (visual case ID) to the lab data for matches:
    labdata[, caseid := casedata$visualId[casematch]]
    
    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check if any first names match with surnames exactly:
    labdata[is.na(caseid), dscore_fn := match(x = firstName, 
                                              table = casedata$lastName)]
    
    # Check if any surnames match with first names exactly:
    labdata[is.na(caseid), dscore_sn := match(x = lastName, 
                                              table = casedata$firstName)]
    
    # Return case match index if all three columns match, else NA:
    labdata[is.na(caseid), casematch := fifelse(test = !is.na(any(dscore_fn, 
                                                                  dscore_sn, 
                                                                  dscore_dob)) & 
                                                  var(c(dscore_fn, 
                                                        dscore_sn, 
                                                        dscore_dob)) == 0, 
                                                yes = dscore_fn, 
                                                no = NA_real_), 
            by = 1:nrow(labdata)]
    
    # Add Go.Data visualId (visual caseid) for any additional matches:
    labdata[is.na(caseid), caseid := casedata$visualId[casematch]]
    
  }
  
  if(matchmethod == "fuzzy" & matchcols == "names & dob"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check if any first names match by soundex codes:
    labdata[, dscore_fn := stringdist::amatch(x = firstName, 
                                              table = casedata$firstName, 
                                              method = "soundex")]
    
    # Check if any surnames match by soundex codes:
    labdata[, dscore_sn := stringdist::amatch(x = lastName, 
                                              table = casedata$lastName, 
                                              method = "soundex")]
    
    # Check if any dates of birth match by Daperau-Levenshtein D1:
    labdata[, dscore_dob := stringdist::amatch(x = dob, 
                                               table = casedata$dob, 
                                               method = "dl", 
                                               maxDist = 1)]
    
    # Return case match index if all three columns match, else NA:
    labdata[, casematch := fifelse(test = !is.na(any(dscore_fn, 
                                                     dscore_sn, 
                                                     dscore_dob)) & 
                                     var(c(dscore_fn, 
                                           dscore_sn, 
                                           dscore_dob)) == 0, 
                                   yes = dscore_fn, 
                                   no = NA_real_), 
            by = 1:nrow(labdata)]
    
    # Add the Go.Data visualId (visual case ID) to the lab data for matches:
    labdata[, caseid := casedata$visualId[casematch]]
    
    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check if any first names match with surnames by soundex codes:
    labdata[is.na(caseid), dscore_fn := stringdist::amatch(x = firstName, 
                                                           table = casedata$lastName, 
                                                           method = "soundex")]
    
    # Check if any surnames match with first names by soundex codes:
    labdata[is.na(caseid), dscore_sn := stringdist::amatch(x = lastName, 
                                                           table = casedata$firstName, 
                                                           method = "soundex")]
    
    # Return case match index if all three columns match, else NA:
    labdata[is.na(caseid), casematch := fifelse(test = !is.na(any(dscore_fn, 
                                                                  dscore_sn, 
                                                                  dscore_dob)) & 
                                                  var(c(dscore_fn, 
                                                        dscore_sn, 
                                                        dscore_dob)) == 0, 
                                                yes = dscore_fn, 
                                                no = NA_real_), 
            by = 1:nrow(labdata)]
    
    # Add Go.Data visualId (visual caseid) for any additional matches:
    labdata[is.na(caseid), caseid := casedata$visualId[casematch]]

  }
  
  if(matchmethod == "exact" & matchcols == "names & age"){
    
    
    
  }
  
  
  # Return updated lab data with Go.Data visual case IDs added for matches:
  return(labdata)
  
}
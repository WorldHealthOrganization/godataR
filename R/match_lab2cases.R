#' Link lab results to Go.Data cases 
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function will perform exact or fuzzy matching to match laboratory 
#' records to Go.Data cases, based on common identifiers that are present
#' in both data sets.
#' 
#' For document ID, it is suggested to use exact matching unless the document 
#' number has at least 8 digits (otherwise fuzzy matching will be very 
#' indiscriminate). If fuzzy matching is selected, a Levenshtein distance of 1 
#' (typographic difference but no transpositions) will be used.
#' 
#' For identifiers that include first names and surnames, if fuzzy matching is 
#' selected, then the names will be converted to soundex codes and matched on 
#' this basis.  Note that soundex codes are less discriminatory with longer 
#' names; it is therefore advisable to include both first name, surname and 
#' date of birth or age if possible, to increase the accuracy of the matches.  
#' 
#' For date of birth, a Damerau-Levenshtein distance of 1 is used (this allows 
#' for a maximum of one transposition, e.g. 21 written as 12).
#' 
#' For age, only exact matching is performed as there are only two digits.
#' 
#' If more than one value within one of the matchcols matches a lab record (e.g.
#' a firstName is duplicated), the final match will be based on the ties between
#' all matchcols (i.e. for firstName, lastName and dob, the final match will be 
#' made with the row index that is represented in all three column matches).  
#' 
#' A data.table solution is used for exact matches rather than
#' `base::match()` as base R match only returns the first match position in the
#' list (credits to contributors to [this StackOverflow post]
#' (https://stackoverflow.com/questions/73613758/get-indices-of-matches-with-a-
#' column-in-a-second-data-table#73614750).
#' 
#' For the sam reason, `EmilMisc::mamatch()` from the Github package created by
#' Emil Bode, which can handle multiple matches, is used instead of 
#' `stringdist::amatch()`.   
#' 
#' Finally, for a match to be returned, the sample collection date for the lab 
#' result must be within the number of days defined by the epiwindow of the 
#' date that the matched case was reported. For example, if the epiwindow is 30
#' days, there must be no more than 30 days between the sample collection date 
#' and the case report (notification) date.
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
#' @param epiwindow maximum number of days between sample date and report date
#' @param matchcols one of "names & dob", "names & age", "names" or "doc ID".
#' @param matchmethod one of "fuzzy" or "exact"
#' 
#' @return updated lab data with Go.data case IDs for identified matches.
#' 
#' @import lubridate
#' @import dplyr
#' @import dtplyr
#' @import data.table
#' @import EmilMisc
#' 
#' @examples 
#' # Load libraries:
#' library(data.table)
#' library(dplyr)
#' library(dtplyr)
#' 
#' # Create example case data:
#' casedata <- data.table(visualId = c("C001", "C002", "C003", "c004", "c005"), 
#'                        documents_number = c("N099", 
#'                                             "N052", 
#'                                             "N047", 
#'                                             "N079", 
#'                                             "N088"), 
#'                                             firstName = c("Sam", 
#'                                                           "Leila",
#'                                                           "Jim",
#'                                                           "Julita", 
#'                                                           "Jim"), 
#'                                             lastName = c("Tracyiek", 
#'                                                          "Crowther", 
#'                                                          "Maputo", 
#'                                                          "Nlange", 
#'                                                          "Dutto"), 
#'                                             dob = c("2001-11-04", 
#'                                                     "1980-07-15", 
#'                                                     "1995-07-14", 
#'                                                     "2017-04-03", 
#'                                                     "2019-08-21"), 
#'                                             age_years = c(23, 45, 42, 5, 19), 
#'                                             dateOfReporting = c("2022-08-02",
#'                                                                 "2022-06-16", 
#'                                                                 "2022-07-30", 
#'                                                                 "2022-08-02", 
#'                                                                 "2022-08-31"), 
#'                                             dateOfOnset = c("2022-08-01", 
#'                                                             "2022-06-10", 
#'                                                             "2022-07-25", 
#'                                                             "2022-07-31", 
#'                                                             NA))
#'                                                             
#' # Convert date columns:
#' casedata <- casedata %>% 
#'   mutate(across(.cols = c(dob, contains("date")), .fns = as.Date)) %>% 
#'   as.data.table()
#' 
#' # Create example lab data: 
#' labdata <- data.table(documents_number = c("N099", "N052", "N047", "N123"), 
#'                       firstName = c("Sam", "Lila", "Jim", "Ena"), 
#'                       lastName = c("Tracik", "Crowther", "Dutto", "Lotuto"),
#'                       dob = c("2001-11-04", 
#'                               "1980-07-15", 
#'                               "2019-08-12", 
#'                               "1978-12-23"), 
#'                       age_years = c(23, 45, 19, 29), 
#'                       result = c("neg", "pos", "neg", "pos"), 
#'                       sampledate = c("2022-08-02", 
#'                                      "2022-08-03", 
#'                                      "2022-08-01", 
#'                                      "2022-08-02"))
#' 
#' # Convert date columns:
#' labdata <- labdata %>% 
#'   mutate(across(.cols = c(dob, contains("date")), .fns = as.Date)) %>% 
#'   as.data.table()
#' 
#' #############################################
#' # Exact match on names and dates of birth:
#' updatelab <- match_lab2cases(labdata = labdata, 
#'                              casedata = casedata, 
#'                              epiwindow = 30,
#'                              matchcols = "names & dob", 
#'                              matchmethod = "exact")
#' 
#' # There are no exact matches - try fuzzy matching instead:
#' updatelab <- match_lab2cases(labdata = labdata, 
#'                              casedata = casedata, 
#'                              epiwindow = 30,
#'                              matchcols = "names & dob", 
#'                              matchmethod = "fuzzy")
#' 
#' # View results (some matches are now returned):
#' updatelab
#' @export
match_lab2cases <- function(labdata, 
                            casedata,
                            epiwindow,
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
      
      mutate(dob = lubridate::parse_date_time(x = dob, orders = c("ymd", 
                                                                  "dmy", 
                                                                  "mdy")))
  
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

    # Return exact matches on combination of firstName, lastName and dob:
    labdata[casedata, casematch := .I, on = .(firstName, lastName, dob)]
    
    # Now update with any matches where firstName and lastName are swapped:
    labdata[casedata, casematch := .I, on = .(firstName = lastName, dob)]
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    
  }
  

    if(matchmethod == "fuzzy" & matchcols == "names & dob"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check first name matches by soundex:
    labdata[, fn := EmilMisc::mamatch(x = firstName, 
                                      table = casedata$firstName, 
                                      method = "soundex", 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]
      
    # Check surname matches by soundex:
    labdata[, ln := EmilMisc::mamatch(x = lastName, 
                                      table = casedata$lastName, 
                                      method = "soundex", 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]
      
    # Check dob matches by Damereau-Levenshtein distance (max of 1 permitted):
    labdata[, bd := EmilMisc::mamatch(x = dob, 
                                      table = casedata$dob, 
                                      method = "dl", 
                                      maxDist = 1, 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]

    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check first name matches by soundex:
    labdata[is.na(fn), fn := EmilMisc::mamatch(x = firstName, 
                                               table = casedata$lastName, 
                                               method = "soundex", 
                                               maxmatch = nrow(casedata), 
                                               returnAs = "list")]
    
    # Check surname matches by soundex:
    labdata[is.na(ln), ln := EmilMisc::mamatch(x = lastName, 
                                               table = casedata$firstName, 
                                               method = "soundex", 
                                               maxmatch = nrow(casedata), 
                                               returnAs = "list")]
    

    # Return index that matches in all three columns:
    labdata[, casematch := Reduce(intersect, list(unlist(fn), 
                                                  unlist(ln), 
                                                  unlist(bd)))]
    
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
  }
  
  
  if(matchmethod == "exact" & matchcols == "names & age"){
    
    #####################################################################
    
    # Return exact matches on combination of firstName, lastName and age:
    labdata[casedata, casematch := .I, on = .(firstName, lastName, age_years)]
    
    # Now update with any matches where firstName and lastName are swapped:
    labdata[casedata, casematch := .I, on = .(firstName = lastName, age_years)]
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]

  }
  
  if(matchmethod == "fuzzy" & matchcols == "names & age"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check first name matches by soundex:
    labdata[, fn := EmilMisc::mamatch(x = firstName, 
                                      table = casedata$firstName, 
                                      method = "soundex", 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]
    
    # Check surname matches by soundex:
    labdata[, ln := EmilMisc::mamatch(x = lastName, 
                                      table = casedata$lastName, 
                                      method = "soundex", 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]
    
    # Check exact age match (too few digits for distance measure):
    labdata[casedata, am := .I, on = .(age_years)]
    

    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check first name matches by soundex:
    labdata[is.na(fn), fn := EmilMisc::mamatch(x = firstName, 
                                               table = casedata$lastName, 
                                               method = "soundex", 
                                               maxmatch = nrow(casedata), 
                                               returnAs = "list")]
    
    # Check surname matches by soundex:
    labdata[is.na(ln), ln := EmilMisc::mamatch(x = lastName, 
                                               table = casedata$firstName, 
                                               method = "soundex", 
                                               maxmatch = nrow(casedata), 
                                               returnAs = "list")]
    
    
    # Return index that matches in all three columns:
    labdata[, casematch := Reduce(intersect, list(unlist(fn), 
                                                  unlist(ln), 
                                                  unlist(am)))]
    
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    
  }
  
  if(matchmethod == "exact" & matchcols == "names"){
    
    #####################################################################
    
    # Return exact matches on combination of firstName and lastName:
    labdata[casedata, casematch := .I, on = .(firstName, lastName)]
    
    # Now update with any matches where firstName and lastName are swapped:
    labdata[casedata, casematch := .I, on = .(firstName = lastName)]
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]

  }
  
  if(matchmethod == "fuzzy" & matchcols == "names"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check first name matches by soundex:
    labdata[, fn := EmilMisc::mamatch(x = firstName, 
                                      table = casedata$firstName, 
                                      method = "soundex", 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]
    
    # Check surname matches by soundex:
    labdata[, ln := EmilMisc::mamatch(x = lastName, 
                                      table = casedata$lastName, 
                                      method = "soundex", 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]
    
    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check first name matches by soundex:
    labdata[is.na(fn), fn := EmilMisc::mamatch(x = firstName, 
                                               table = casedata$lastName, 
                                               method = "soundex", 
                                               maxmatch = nrow(casedata), 
                                               returnAs = "list")]
    
    # Check surname matches by soundex:
    labdata[is.na(ln), ln := EmilMisc::mamatch(x = lastName, 
                                               table = casedata$firstName, 
                                               method = "soundex", 
                                               maxmatch = nrow(casedata), 
                                               returnAs = "list")]
    
    
    # Return index that matches in all three columns:
    labdata[, casematch := Reduce(intersect, list(unlist(fn), 
                                                  unlist(ln)))]
    
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]

  }
  
  if(matchmethod == "exact" & matchcols == "doc ID"){
    
    ####################################################################
    
    # Return exact matches on document number:
    labdata[casedata, casematch := .I, on = .(documents_number)]

    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    

  }
  
  if(matchmethod == "fuzzy" & matchcols == "doc ID"){
    
    ####################################################################
    
    # Check document number matches by Damereau-Levenshtein distance (max of 1):
    labdata[, did := EmilMisc::mamatch(x = documents_number, 
                                      table = casedata$documents_number, 
                                      method = "lv", 
                                      maxDist = 1, 
                                      maxmatch = nrow(casedata), 
                                      returnAs = "list")]
    

    # Return index that matches in all three columns:
    labdata[, casematch := fifelse(length(unlist(did)) == 1, 
                                   lapply(did, unlist),
                                   list(NA_real_)), 
            by = 1:nrow(labdata)]
    
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epicheck := abs(sampledate - casedata$dateOfReporting[casematch])
            <= epiwindow]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    
  }
  
  
  
  # Return updated lab data with Go.Data visual case IDs added for matches:
  return(labdata)
  
}
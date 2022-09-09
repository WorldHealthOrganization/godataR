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
#' (typographic difference but no transpositions) will be used.  If more than 
#' one document ID matches with fuzzy matching, a result of 'no match'
#' will be returned in the Go.Data case ID column.
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
#' For age in years, only exact matching is performed.
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
#' For the same reason, `EmilMisc::mamatch()` from the Github package created by
#' Emil Bode, which can return multiple matches, is used instead of 
#' `stringdist::amatch()` which only returns the first match.   
#' 
#' Finally, for a match to be returned, the sample collection date for the lab 
#' result must be within the number of days defined by the epiwindow of the 
#' date that the matched case was reported. For example, if the epiwindow is 30
#' days, there must be no more than 30 days between the lab sample collection 
#' date and the case notification date (which is called dateOfReporting in 
#' Go.Data and is a mandatory data entry field in all case investigation forms).
#' 
#' In order to return the Go.Data case ID for a match, all the selected 
#' identifiers must match under the selected criteria. For example: if first 
#' name, last name and birth date were selected (matchcols = names & dob) with 
#' fuzzy matching (matchmethod = 'fuzzy'), matched cases would have to have:
#'   + the same soundex codes for first names (allowing firstName == lastName) 
#'   + the same soundex codes for last names (allowing lastName == firstName)
#'   + the same date of birth (allowing for a maximum of one transposition) 
#'   + lab sample collection date within epiwindow of case dateOfReporting
#' 
#' Note that the relevant column name arguments must be included for each match
#' method protocol.  For example, if matchcols = 'names & dob', a character 
#' string with the names of the relevant columns must be supplied for each of 
#' 'firstnamecol', 'lastnamecol' and 'dobcol'.  This is to allow mapping of 
#' those columns in the lab data to their counterparts in Go.Data case data.
#' 
#' @md
#'  
#' @param labdata data.frame of laboratory records to match to Go.Data cases
#' @param casedata data.frame of case data imported from Go.Data
#' @param epiwindow maximum number of days between sample date and report date
#' @param matchcols one of "names & dob", "names & age", "names" or "doc ID".
#' @param firstnamecol name of lab column containing first names (character)
#' @param lastnamecol name of lab column containing last names (character)
#' @param dobcol name of lab column containing dates of birth (character)
#' @param agecol name of lab column containing age in years (character)
#' @param docidcol name of lab column containing document ID numbers (character)
#' @param sampledatecol name of lab column containing specimen collection dates
#' @param casereportdatecol name of case column containing case report dates
#' @param matchmethod one of "fuzzy" or "exact"
#' 
#' @return updated lab data with Go.data case IDs for identified matches as 
#' well as other columns showing a breakdown of which elements matched.
#' 
#' @import lubridate
#' @import data.table
#' @import EmilMisc
#' 
#' @examples 
#' 
#' # Create example case data:
#' casedata <- data.frame(visualId = c("C001", 
#'                                     "C002", 
#'                                     "C003", 
#'                                     "c004", 
#'                                     "c005"), 
#'                        documents_number = c("N099", 
#'                                             "N052", 
#'                                             "N047", 
#'                                             "N079", 
#'                                             "N088", 
#'                                             "N092"), 
#'                                             firstName = c("Sam", 
#'                                                           "Leila",
#'                                                           "Jim",
#'                                                           "Julita", 
#'                                                           "Jim", 
#'                                                           "Lotuto), 
#'                                             lastName = c("Tracyiek", 
#'                                                          "Crowther", 
#'                                                          "Maputo", 
#'                                                          "Nlange", 
#'                                                          "Dutto", 
#'                                                          "Ena"), 
#'                                             dob = c("2001-11-04", 
#'                                                     "1980-07-15", 
#'                                                     "1995-07-14", 
#'                                                     "2017-04-03", 
#'                                                     "2019-08-21", 
#'                                                     "1978-12-23"), 
#'                                             age_years = c(23,45,42,5,19,44), 
#'                                             dateOfReporting = c("2022-08-02",
#'                                                                 "2022-06-16", 
#'                                                                 "2022-07-30", 
#'                                                                 "2022-08-02", 
#'                                                                 "2022-08-31", 
#'                                                                 "2022-07-31"), 
#'                                             dateOfOnset = c("2022-08-01", 
#'                                                             "2022-06-10", 
#'                                                             "2022-07-25", 
#'                                                             "2022-07-31", 
#'                                                             NA, 
#'                                                             "2022-07-29"))
#'                                                             
#' # Create example lab data: 
#' labdata <- data.frame(documents_number = c("N099", "N052", "N088", "N091"), 
#'                       firstname = c("Sam", "Lila", "Jim", "Ena"), 
#'                       surname = c("Tracik", "Crowther", "Dutto", "Lotuto"),
#'                       dob = c("2001-11-04", 
#'                               "1980-07-15", 
#'                               "2019-08-12", 
#'                               "1978-12-23"), 
#'                       age = c(23, 45, 19, 44), 
#'                       result = c("neg", "pos", "neg", "pos"), 
#'                       sampledate = c("2022-08-02", 
#'                                      "2022-08-03", 
#'                                      "2022-08-01", 
#'                                      "2022-08-02"))
#' 
#' #############################################
#' # Exact match on names and dates of birth:
#' updatelab <- match_lab2cases(labdata = labdata, 
#'                              casedata = casedata, 
#'                              epiwindow = 30,
#'                              matchcols = "names & dob",
#'                              sampledatecol = "sampledate", 
#'                              firstnamecol = "firstname",
#'                              lastnamecol = "surname",
#'                              dobcol = "dob",
#'                              matchmethod = "exact")
#' 
#' # There is one match for the last case (after firstName and lastName columns 
#' # are swapped between the two data sets they are found to match for case 6).
#' 
#' # Now try fuzzy matching on names and dates of birth:
#' updatelab <- match_lab2cases(labdata = labdata, 
#'                              casedata = casedata, 
#'                              epiwindow = 30,
#'                              matchcols = "names & dob", 
#'                              sampledatecol = "sampledate",
#'                              firstnamecol = "firstname",
#'                              lastnamecol = "surname",
#'                              dobcol = "dob",
#'                              matchmethod = "fuzzy")
#' 
#' # Now 3/4 lab records are fuzzy matched to a case in casedata
#' # Note that lab record 2 is not matched because it did't fall within the 
#' # epiwindow.
#' # Note that lab record 3 matches two cases with the same first name, but only
#' # one case last name and date of birth (case 5) so case 5 is returned as the
#' # unique match.
#' 
#' # View results (matched cases have a Go.Data case ID):
#' updatelab
#' @export
match_lab2cases <- function(labdata, 
                            casedata,
                            epiwindow,
                            matchcols = c("names & dob", 
                                          "names & age", 
                                          "names", 
                                          "doc ID"), 
                            firstnamecol = NULL,
                            lastnamecol = NULL,
                            dobcol = NULL,
                            agecol = NULL,
                            docidcol = NULL,
                            sampledatecol,
                            casereportdatecol = "dateOfReporting",
                            matchmethod = c("fuzzy", 
                                            "exact")){
  
  ###########################
  # 0. Prepare data sets:
  ###########################

  # Convert case and lab data to data.table 
  # to improve speed and memory consumption of matching process
  
  # Convert case data to data.table:
  casedata =  as.data.table(casedata)
  
  # Convert lab data tp data.table:
  labdata = as.data.table(labdata)
  

  ###################################
  # 1. Check & format match columns:
  ###################################
  
  # Check that the names of the columns to match on have been supplied:
  
  if(matchcols == "names & dob" 
     & (is.null(firstnamecol) 
        | is.null(lastnamecol) 
        | is.null(dobcol)
        | is.null(sampledatecol))){
       
       stop("Column names to match on are missing.\n If you want to match on 'names & dob', specify firstnamecol, lastnamecol, dobcol and sampledatecol.")
       
  } else if(matchcols == "names & age" 
            & (is.null(firstnamecol) 
               | is.null(lastnamecol) 
               | is.null(agecol)
               | is.null(sampledatecol))){
    
    stop("Column names to match on are missing.\n If you want to match on 'names & age', specify firstnamecol, lastnamecol, agecol and sampledatecol.")
    
  } else if(matchcols == "names" 
            & (is.null(firstnamecol) 
               | is.null(lastnamecol)
               | is.null(sampledatecol))){
    
    stop("Column names to match on are missing.\n If you want to match on 'names' only, specify firstnamecol, lastnamecol and sampledatecol.")
    
  } else if(matchcols == "doc ID" 
            & (is.null(docidcol)
               | is.null(sampledatecol))){
    
    stop("Column names to match on are missing.\n If you want to match on 'doc ID' only, specify docidcol and sampledatecol.")
    
  } else {
    
    # Map case report date column if case data is not from Go.Data:
    if(casereportdatecol != "dateOfReporting"){
      
      data.table::setnames(x = casedata, 
                           old = casereportdatecol, 
                           new = "dateOfReporting")
      
    } 
    
    # Map lab column names to facilitate rest of code:
    if(matchcols == "names & dob"){
      
      data.table::setnames(x = labdata, 
                           old = c(firstnamecol, lastnamecol, dobcol, sampledatecol), 
                           new = c("firstName", "lastName", "dob", "sampledate"))
      
    } else if(matchcols == "names & age"){
      
      data.table::setnames(x = labdata, 
                           old = c(firstnamecol, lastnamecol, agecol, sampledatecol), 
                           new = c("firstName", "lastName", "age_years", "sampledate"))
      
    } else if(matchcols == "names"){
      
      data.table::setnames(x = labdata, 
                           old = c(firstnamecol, lastnamecol, sampledatecol), 
                           new = c("firstName", "lastName", "sampledate"))
      
    } else if(matchcols == "doc ID"){
      
      data.table::setnames(x = labdata, 
                           old = c(docidcol, sampledatecol), 
                           new = c("documents_number", "sampledate"))
      
    }
    
  }
  
  ########################################################################
  # Check that supplied columns are in the correct format:
  
  # Check sample dates are in the correct format and convert if not:
  labdata[, sampledate := lubridate::parse_date_time(x = sampledate, 
                                                     orders = c("ymd", 
                                                                "dmy", 
                                                                "mdy"))]
  # Convert sample date from POSIXct to date:
  labdata[, sampledate := as.Date(sampledate)]
  
  # Check that dateOfReporting is in the correct format and convert if not:
  casedata[, dateOfReporting := lubridate::parse_date_time(x = dateOfReporting, 
                                                           orders = c("ymd", 
                                                                      "dmy", 
                                                                      "mdy"))]
  # Convert dateOfReporting from POSIXct to date:
  casedata[, dateOfReporting := as.Date(dateOfReporting)]
  
  
  if(matchcols == "names & dob"){
    
    # Check that birthdates in lab data are formatted as dates, convert if not:
    labdata[, dob := lubridate::parse_date_time(x = dob, orders = c("ymd", 
                                                                    "dmy", 
                                                                    "mdy"))]
    
    # Check that birthdates in case data are formatted as dates, convert if not:
    casedata[, dob := lubridate::parse_date_time(x = dob, orders = c("ymd", 
                                                                     "dmy", 
                                                                     "mdy"))]
    
  
  } else if(matchcols == "names & age"){
    
    # Make sure age (years) in lab data is integer:
    labdata[, age_years := as.integer(age_years)]
    
    # Make sure age (years) in case data is integer:
    casedata[, age_years := as.integer(age_years)]
    
    
  } else if(matchcols == "doc ID"){
    
    # Convert lab data to lower case and strip white spaces:
    labdata[, documents_number := tolower(gsub(pattern = " ", 
                                               replacement = "", 
                                               x = documents_number))]
    
    # Convert case data to lower case and strip white spaces:
    casedata[, documents_number := tolower(gsub(pattern = " ", 
                                                replacement = "", 
                                                x = documents_number))]
    
  }
  
  
  ###########################
  # 2. Perform matching:
  ###########################
  
  
  if(matchmethod == "exact" & matchcols == "names & dob"){
    
    #####################################################################

    # Return exact matches on combination of firstName, lastName and dob:
    labdata[casedata, casematch := .I, on = .(firstName, lastName, dob)]
    
    # Now update with any matches where firstName and lastName are swapped:
    labdata[casedata, casematch := .I, on = .(firstName = lastName, 
                                              lastName = firstName, 
                                              dob)]
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epidatediff := abs(sampledate - 
                                   casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]

  } else if(matchmethod == "fuzzy" & matchcols == "names & dob"){
    
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
    labdata[, epidatediff := abs(sampledate - casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    
  } else if(matchmethod == "exact" & matchcols == "names & age"){
    
    #####################################################################
    
    # Return exact matches on combination of firstName, lastName and age:
    labdata[casedata, casematch := .I, on = .(firstName, lastName, age_years)]
    
    # Now update with any matches where firstName and lastName are swapped:
    labdata[casedata, casematch := .I, on = .(firstName = lastName,
                                              lastName = firstName, 
                                              age_years)]
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epidatediff := abs(sampledate - 
                                   casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]

  } else if(matchmethod == "fuzzy" & matchcols == "names & age"){
    
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
    labdata[, epidatediff := abs(sampledate - 
                                   casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    
  } else if(matchmethod == "exact" & matchcols == "names"){
    
    #####################################################################
    
    # Return exact matches on combination of firstName and lastName:
    labdata[casedata, casematch := .I, on = .(firstName, lastName)]
    
    # Now update with any matches where firstName and lastName are swapped:
    labdata[casedata, casematch := .I, on = .(firstName = lastName, 
                                              lastName = firstName)]
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epidatediff := abs(sampledate - 
                                   casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]

  } else if(matchmethod == "fuzzy" & matchcols == "names"){
    
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
    labdata[, epidatediff := abs(sampledate - 
                                   casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]

  } else if(matchmethod == "exact" & matchcols == "doc ID"){
    
    ####################################################################
    
    # Return exact matches on document number:
    labdata[casedata, casematch := .I, on = .(documents_number)]
    
    # Next, check that specimen dates are within range of epiwindow:
    labdata[, epidatediff := abs(sampledate - 
                                   casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    

  } else if(matchmethod == "fuzzy" & matchcols == "doc ID"){
    
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
    labdata[, epidatediff := abs(sampledate - 
                                   casedata$dateOfReporting[casematch])]
    
    # Compare epidatediff with epiwindow:
    labdata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                  yes = TRUE, 
                                  no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    labdata[, godata_cid := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                    yes = casedata$visualId[casematch],
                                    no = "no match")]
    
  }
  
  # Return updated lab data with Go.Data visual case IDs added for matches:
  return(labdata)
  
}
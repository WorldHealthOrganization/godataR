#' Link two patient data sets with fuzzy or exact matching 
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function will perform exact or fuzzy matching to link two data sets, 
#' based on patient demographic columns that must be present in both. 
#' 
#' This function requires two data sets as input:
#'   + 'Base' data set (the data set you want to return the matches for)
#'   + 'Lookup table' (the data set you want to retrieve the matches from)
#' 
#' The following column types are used for matching, either individually or in
#' combination:
#' 
#'   1. Patient first name
#'   2. Patient last name
#'   3. Patient date of birth
#'   4. Patient age in years
#'   5. Patient identity number (e.g. from social security card or passport)
#'   6. Patient notification date (compared with date from second data set)
#' 
#' The following combinations are available as match criteria:
#' 
#'   + `names & dob`: patient first name, last name and date of birth
#'   + `names & age`: patient first name, last name and age in years
#'   + `names`: patient first name and last name
#'   + `doc ID`: patient national identity card number
#' 
#' In addition, all matches returned with these criteria are further tested to 
#' determine if they fall within the selected date window (`epiwindow`).  For 
#' this reason, it is necessary to specify a date column to compare in both data
#' sets, i.e. `basedatecol` for the base data set and `lookupdatecol` for the 
#' data set to retrieve the matches from.
#' 
#' The function has been designed with the following use cases in mind:
#' 
#'   1. Laboratory results (base data set) that need to be linked with Go.Data 
#'   case IDs (lookup table) so that they can be imported into Go.Data in bulk;
#'   2. Sequencing results (base data set) that need to be linked with existing
#'   lab records in Go.Data (lookup table) so that these records can be updated.
#' 
#' The lookup tables for these scenarios can be imported from Go.Data with the
#'  `get_cases_epiwindow()` and `get_labresults_epiwindow()` functions, 
#'  respectively, which are both available in this package (godataR).
#' 
#' @details 
#' **Broad approach:**
#' As fuzzy matching can be computationally intensive, both input tables (base 
#' and lookup) are first converted to data.table.  
#' 
#' Exact matches are performed using data.table syntax, which should be very 
#' fast, even for large data sets.  Further information on the syntax used is 
#' available in [this StackOverflow post](https://stackoverflow.com/questions/73
#' 613758/get-indices-of-matches-with-a-column-in-a-second-data-table#73614750).
#' 
#' Fuzzy matches are performed using `EmilMisc::mamatch()` from the package 
#' created by Emil Bode (available on GitHub), which can return multiple matches
#' if match columns contain duplicated values. This is an extension of the
#' `stringdist::amatch()` function which only returns the first match. 
#'   
#' 
#' **Linking by first name and last name:**
#' Patient names (first name and last name) are converted to soundex codes for 
#' fuzzy matching.  At present, this facility is only available for data written
#' in the English alphabet; future versions will include a character translator 
#' to accommodate other text types. Note that the more syllables a name contains,
#' the less discriminatory soundex codes will be.  It is therefore advisable to 
#' combine fuzzy matching on names with date of birth or age if possible, as 
#' this will increase the specificity of the matches. You may also wish to test
#' the different match protocols in this function on a test data set with known
#' matches, before choosing which protocol to use in a routine workflow. 
#' 
#' For both fuzzy and exact matching, first and last names are first compared 
#' with their counterparts in the lookup table.  Then first names from the base
#' data are compared with last names from the lookup table, and vice versa, to 
#' match any names that may have been recorded in the wrong order.
#' 
#' **Linking by date of birth:**
#' For date of birth, a Damerau-Levenshtein distance of 1 is used (this allows 
#' for a maximum of one transposition, e.g. 21 written as 12).  Transpositions
#' in dates of birth are relatively common typographic errors.
#' 
#' **Linking by age:**
#' If dates of birth are not available, age in years can be used in combination
#' with first and last name instead.  Only exact matches are performed on age, 
#' as the number of digits is too small to reliably facilitate any kind of 
#' fuzzy distance matching.
#' 
#' **Linking by document number:**
#' If a national identity number (such as social security number, hospital 
#' number or passport number) is present in both data sets, this ID number can 
#' be used for matching instead of names and dates of birth or age.  Often this 
#' type of number may be unique to the patient but not necessarily unique to 
#' their current case episode of illness.  For this reason (as with all the 
#' other match protocols) matches will only be returned for cases from the 
#' lookup table that are also within the user-defined epidemiological window.
#' 
#' Both exact and fuzzy matching are available for document ID.  Fuzzy matching
#' is performed with a Damerau-Levenshtein distance of 1 (allowing for one 
#' transposition).  Users are advised to perform exact matches on document 
#' number in the first instance, unless the number comprises of at least eight 
#' digits.  Shorter numbers may return too many matches that meet the DL = 1 
#' criteria.  If a single match cannot be identified from all the criteria in 
#' the match protocol, a result of 'no match' will be returned.    
#' 
#' **Linking by episode window:**
#' This function requires matches based on patient demographics to also fall 
#' within a user-defined episode window (maximum number of days allowed 
#' between a date in the base data set and a date in the lookup table to 
#' confirm the match).  How the episode window is defined depends on the two 
#' dates available for comparison.  For example, if using specimen date for lab 
#' results and comparing this with case notification date in Go.Data, it may be
#' useful to use the number of days that distinguish ongoing infections from 
#' reinfections, especially if there are some known reinfections in the data.
#' The appropriate episode window to use will depend on the pathogen and the 
#' nature of the outbreak under investigation, as well as national and 
#' international case definitions for reinfection.  For other scenarios, a 
#' simpler approach may suffice (such as taking the average time in days between
#' case notification and specimen collection dates).
#' 
#' **Dealing with multiple matches:**
#' If more than one match is returned for any column (e.g. if a first name is 
#' present more than once in the lookup table), the final match will be based on
#' the ties between all match columns (e.g. if matching on firstName, lastName 
#' and dob, the final match will be made with the row index that is represented 
#' in all three column matches).  

#' @md
#'  
#' @param basedata data.frame of patient records to match to another data set
#' @param lookuptable data.frame of patient records to retrieve matches from
#' @param epiwindow maximum number of days between sample date and report date
#' @param matchcols one of "names & dob", "names & age", "names" or "doc ID".
#' @param firstnamecol name of lab column containing first names (character)
#' @param lastnamecol name of lab column containing last names (character)
#' @param dobcol name of lab column containing dates of birth (character)
#' @param agecol name of lab column containing age in years (character)
#' @param docidcol name of lab column containing document ID numbers (character)
#' @param basedatecol name of date column in base data to use for epiwindow
#' @param lookupdatecol name of date column in lookup table to use for epiwindow
#' @param lookupmatchcol name of ID column in lookup table to return matches of
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
#' updatelab <- match_cases(basedata = labdata, 
#'                          lookuptable = casedata, 
#'                          epiwindow = 30,
#'                          matchcols = "names & dob",
#'                          basedatecol = "sampledate", 
#'                          firstnamecol = "firstname",
#'                          lastnamecol = "surname",
#'                          dobcol = "dob",
#'                          matchmethod = "exact")
#' 
#' # There is one match for the last case (after firstName and lastName columns 
#' # are swapped between the two data sets they are found to match for case 6).
#' 
#' # Now try fuzzy matching on names and dates of birth:
#' updatelab <- match_cases(basedata = labdata, 
#'                          lookuptable = casedata, 
#'                          epiwindow = 30,
#'                          matchcols = "names & dob", 
#'                          basedatecol = "sampledate",
#'                          lookupdatecol = "dateOfReporting",
#'                          lookupmatchcol = "visualId",
#'                          firstnamecol = "firstname",
#'                          lastnamecol = "surname",
#'                          dobcol = "dob",
#'                          matchmethod = "fuzzy")
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
match_cases <- function(basedata, 
                        lookuptable,
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
                        basedatecol,
                        lookupdatecol = "dateOfReporting",
                        lookupmatchcol = "visualId",
                        matchmethod = c("fuzzy", 
                                        "exact")){
  
  ###########################
  # 0. Prepare data sets:
  ###########################

  # Convert lookup and base data to data.table 
  # to improve speed and memory consumption of matching process
  
  # Convert lookup table to data.table:
  lookuptable =  as.data.table(lookuptable)
  
  # Convert base data to data.table:
  basedata = as.data.table(basedata)
  

  ###################################
  # 1. Check & format match columns:
  ###################################
  
  # Check that the names of the columns to match on have been supplied:
  
  if(matchcols == "names & dob" 
     & (is.null(firstnamecol) 
        | is.null(lastnamecol) 
        | is.null(dobcol)
        | is.null(basedatecol))){
       
       stop("Column names to match on are missing.\n If you want to match on 'names & dob', specify firstnamecol, lastnamecol, dobcol and basedatecol.")
       
  } else if(matchcols == "names & age" 
            & (is.null(firstnamecol) 
               | is.null(lastnamecol) 
               | is.null(agecol)
               | is.null(basedatecol))){
    
    stop("Column names to match on are missing.\n If you want to match on 'names & age', specify firstnamecol, lastnamecol, agecol and basedatecol.")
    
  } else if(matchcols == "names" 
            & (is.null(firstnamecol) 
               | is.null(lastnamecol)
               | is.null(basedatecol))){
    
    stop("Column names to match on are missing.\n If you want to match on 'names' only, specify firstnamecol, lastnamecol and basedatecol.")
    
  } else if(matchcols == "doc ID" 
            & (is.null(docidcol)
               | is.null(basedatecol))){
    
    stop("Column names to match on are missing.\n If you want to match on 'doc ID' only, specify docidcol and basedatecol.")
    
  } else {
    
    # Map lookup date column:
    data.table::setnames(x = lookuptable, 
                         old = lookupdatecol, 
                         new = "lookupdate")
    
    # Map lookup primary ID column to return for matches:
    data.table::setnames(x = lookuptable, 
                         old = lookupmatchcol, 
                         new = "match_id")
    
    # Map base column names to facilitate rest of code:
    if(matchcols == "names & dob"){
      
      data.table::setnames(x = basedata, 
                           old = c(firstnamecol, lastnamecol, dobcol, basedatecol), 
                           new = c("firstName", "lastName", "dob", "basedate"))
      
    } else if(matchcols == "names & age"){
      
      data.table::setnames(x = basedata, 
                           old = c(firstnamecol, lastnamecol, agecol, basedatecol), 
                           new = c("firstName", "lastName", "age_years", "basedate"))
      
    } else if(matchcols == "names"){
      
      data.table::setnames(x = basedata, 
                           old = c(firstnamecol, lastnamecol, basedatecol), 
                           new = c("firstName", "lastName", "basedate"))
      
    } else if(matchcols == "doc ID"){
      
      data.table::setnames(x = basedata, 
                           old = c(docidcol, basedatecol), 
                           new = c("documents_number", "basedate"))
      
    }
    
  }
  
  ########################################################################
  # Check that supplied columns are in the correct format:
  
  # Check base dates are in the correct format and convert if not:
  basedata[, basedate := lubridate::parse_date_time(x = basedate, 
                                                    orders = c("ymd", 
                                                               "dmy", 
                                                               "mdy"))]
  # Convert base date from POSIXct to date:
  basedata[, basedate := as.Date(basedate)]
  
  # Check that lookupdate is in the correct format and convert if not:
  lookuptable[, lookupdate := lubridate::parse_date_time(x = lookupdate, 
                                                         orders = c("ymd", 
                                                                    "dmy", 
                                                                    "mdy"))]
  # Convert lookupdate from POSIXct to date:
  lookuptable[, lookupdate := as.Date(lookupdate)]
  
  
  if(matchcols == "names & dob"){
    
    # Check that birthdates in lab data are formatted as dates, convert if not:
    basedata[, dob := lubridate::parse_date_time(x = dob, 
                                                 orders = c("ymd", 
                                                            "dmy", 
                                                            "mdy"))]
    
    # Check that birthdates in case data are formatted as dates, convert if not:
    lookuptable[, dob := lubridate::parse_date_time(x = dob, 
                                                    orders = c("ymd", 
                                                               "dmy", 
                                                               "mdy"))]
    
  
  } else if(matchcols == "names & age"){
    
    # Make sure age (years) in lab data is integer:
    basedata[, age_years := as.integer(age_years)]
    
    # Make sure age (years) in case data is integer:
    lookuptable[, age_years := as.integer(age_years)]
    
    
  } else if(matchcols == "doc ID"){
    
    # Convert base data document ID to lower case and strip white spaces:
    basedata[, documents_number := tolower(gsub(pattern = " ", 
                                                replacement = "", 
                                                x = documents_number))]
    
    # Convert lookup table document ID to lower case and strip white spaces:
    lookuptable[, documents_number := tolower(gsub(pattern = " ", 
                                              replacement = "", 
                                              x = documents_number))]
    
  }
  
  
  ###########################
  # 2. Perform matching:
  ###########################
  
  
  if(matchmethod == "exact" & matchcols == "names & dob"){
    
    #####################################################################

    # Return exact matches on combination of firstName, lastName and dob:
    basedata[lookuptable, casematch := .I, on = .(firstName, lastName, dob)]
    
    # Now update with any matches where firstName and lastName are swapped:
    basedata[lookuptable, casematch := .I, on = .(firstName = lastName, 
                                                  lastName = firstName, 
                                                  dob)]
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - 
                                    lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]

  } else if(matchmethod == "fuzzy" & matchcols == "names & dob"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check first name matches by soundex:
    basedata[, fn := EmilMisc::mamatch(x = firstName, 
                                       table = lookuptable$firstName, 
                                       method = "soundex", 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]
      
    # Check surname matches by soundex:
    basedata[, ln := EmilMisc::mamatch(x = lastName, 
                                       table = lookuptable$lastName, 
                                       method = "soundex", 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]
      
    # Check dob matches by Damereau-Levenshtein distance (max of 1 permitted):
    basedata[, bd := EmilMisc::mamatch(x = dob, 
                                       table = lookuptable$dob, 
                                       method = "dl", 
                                       maxDist = 1, 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]

    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check first name matches by soundex:
    basedata[is.na(fn), fn := EmilMisc::mamatch(x = firstName, 
                                                table = lookuptable$lastName, 
                                                method = "soundex", 
                                                maxmatch = nrow(lookuptable), 
                                                returnAs = "list")]
    
    # Check surname matches by soundex:
    basedata[is.na(ln), ln := EmilMisc::mamatch(x = lastName, 
                                                table = lookuptable$firstName, 
                                                method = "soundex", 
                                                maxmatch = nrow(lookuptable), 
                                                returnAs = "list")]
    

    # Return index that matches in all three columns:
    basedata[, casematch := Reduce(intersect, list(unlist(fn), 
                                                   unlist(ln), 
                                                   unlist(bd))), 
             by = 1:nrow(basedata)]
    
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]
    
  } else if(matchmethod == "exact" & matchcols == "names & age"){
    
    #####################################################################
    
    # Return exact matches on combination of firstName, lastName and age:
    basedata[lookuptable, casematch := .I, on = .(firstName, lastName, age_years)]
    
    # Now update with any matches where firstName and lastName are swapped:
    basedata[lookuptable, casematch := .I, on = .(firstName = lastName,
                                                  lastName = firstName, 
                                                  age_years)]
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - 
                                   lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]

  } else if(matchmethod == "fuzzy" & matchcols == "names & age"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check first name matches by soundex:
    basedata[, fn := EmilMisc::mamatch(x = firstName, 
                                       table = lookuptable$firstName, 
                                       method = "soundex", 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]
    
    # Check surname matches by soundex:
    basedata[, ln := EmilMisc::mamatch(x = lastName, 
                                       table = lookuptable$lastName, 
                                       method = "soundex", 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]
    
    # Check exact age match (too few digits for distance measure):
    basedata[lookuptable, am := .I, on = .(age_years)]
    

    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check first name matches by soundex:
    basedata[is.na(fn), fn := EmilMisc::mamatch(x = firstName, 
                                                table = lookuptable$lastName, 
                                                method = "soundex", 
                                                maxmatch = nrow(lookuptable), 
                                                returnAs = "list")]
    
    # Check surname matches by soundex:
    basedata[is.na(ln), ln := EmilMisc::mamatch(x = lastName, 
                                                table = lookuptable$firstName, 
                                                method = "soundex", 
                                                maxmatch = nrow(lookuptable), 
                                                returnAs = "list")]
    
    
    # Return index that matches in all three columns:
    basedata[, casematch := Reduce(intersect, list(unlist(fn), 
                                                   unlist(ln), 
                                                   unlist(am))), 
             by = 1:nrow(basedata)]
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - 
                                   lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]
    
  } else if(matchmethod == "exact" & matchcols == "names"){
    
    #####################################################################
    
    # Return exact matches on combination of firstName and lastName:
    basedata[lookuptable, casematch := .I, on = .(firstName, lastName)]
    
    # Now update with any matches where firstName and lastName are swapped:
    basedata[lookuptable, casematch := .I, on = .(firstName = lastName, 
                                                  lastName = firstName)]
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - 
                                   lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]

  } else if(matchmethod == "fuzzy" & matchcols == "names"){
    
    #####################################################################
    ### First iteration: assume firstname and surname in correct columns:
    
    # Check first name matches by soundex:
    basedata[, fn := EmilMisc::mamatch(x = firstName, 
                                       table = lookuptable$firstName, 
                                       method = "soundex", 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]
    
    # Check surname matches by soundex:
    basedata[, ln := EmilMisc::mamatch(x = lastName, 
                                       table = lookuptable$lastName, 
                                       method = "soundex", 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]
    
    ####################################################################
    ### Second iteration with first and surname swapped for non matches:
    
    # Check first name matches by soundex:
    basedata[is.na(fn), fn := EmilMisc::mamatch(x = firstName, 
                                                table = lookuptable$lastName, 
                                                method = "soundex", 
                                                maxmatch = nrow(lookuptable), 
                                                returnAs = "list")]
    
    # Check surname matches by soundex:
    basedata[is.na(ln), ln := EmilMisc::mamatch(x = lastName, 
                                                table = lookuptable$firstName, 
                                                method = "soundex", 
                                                maxmatch = nrow(lookuptable), 
                                                returnAs = "list")]
    
    
    # Return index that matches in both columns:
    basedata[, casematch := Reduce(intersect, list(unlist(fn), 
                                                   unlist(ln))), 
             by = 1:nrow(basedata)]
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - 
                                   lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]

  } else if(matchmethod == "exact" & matchcols == "doc ID"){
    
    ####################################################################
    
    # Return exact matches on document number:
    basedata[lookuptable, casematch := .I, on = .(documents_number)]
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - 
                                   lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]
    

  } else if(matchmethod == "fuzzy" & matchcols == "doc ID"){
    
    ####################################################################
    
    # Check document number matches by Damereau-Levenshtein distance (max of 1):
    basedata[, did := EmilMisc::mamatch(x = documents_number, 
                                       table = lookuptable$documents_number, 
                                       method = "lv", 
                                       maxDist = 1, 
                                       maxmatch = nrow(lookuptable), 
                                       returnAs = "list")]
    

    # Return index that matches in all three columns:
    basedata[, casematch := fifelse(test = (length(unlist(did)) == 1), 
                                    yes = lapply(did, unlist),
                                    no = list(NA_real_)), 
            by = 1:nrow(basedata)]
    
    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := abs(basedate - 
                                   lookuptable$lookupdate[casematch])]
    
    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = epidatediff <= epiwindow, 
                                   yes = TRUE, 
                                   no = FALSE)]
    
    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := fifelse(test = !is.na(casematch) & epicheck == TRUE, 
                                     yes = lookuptable$match_id[casematch],
                                     no = "no match")]
    
  }
  
  # Return updated lab data with Go.Data visual case IDs added for matches:
  return(basedata)
  
}
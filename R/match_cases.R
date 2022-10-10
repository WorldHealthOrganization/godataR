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
#' The lookup tables can be imported from Go.Data with `get_cases_epiwindow()`
#' and `get_labresults_epiwindow()` functions, respectively, which are both
#' available in this package (godataR).
#'
#' The following column types are used for matching, either individually or in
#' combination:
#'
#'   1. Patient first name
#'   2. Patient last name
#'   3. Patient date of birth
#'   4. Patient age in years
#'   5. Patient identity number (e.g. from social security card or passport)
#'   6. Patient record date (compared with date from second data set)
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
#'   1. Add new laboratory results (base data set) to existing cases in Go.Data
#'   (look up table); match is with Go.Data case IDs
#'   2. Update existing lab records in Go.Data (lookup table) with new
#'   sequencing results (base data set); match is with Go.Data lab record IDs
#'   3. Edit existing lab records in Go.Data (lookup table) with different
#'   values (base data); match is with Go.Data lab record IDs
#'
#' The function can also be used to map other data sets, provided the same
#' column types are provided for matching.
#'
#' When matching is being performed to link new records (e.g. new lab results),
#' only the match ID from the lookup table is appended.
#'
#' If matching is being performed to update or edit lab records, both the
#' match ID for the records to be updated and a date time stamp
#' (godata_updatedat) are returned.  When performing bulk imports to Go.Data,
#' this date time stamp needs to be mapped to the 'Updated on' column (which
#' tells Go.Data to replace existing values instead of creating a new record).
#'
#' Adding new sequencing results to existing lab records is a special case; in
#' addition to the match IDs and the date time stamp, a column indicating that
#' sequencing has been performed is added (`sequenced = TRUE`).  When importing
#' the results to Go.Data, this column should be mapped to 'Has sequencing'.
#'
#'
#' @details
#' **Broad approach:**
#' As fuzzy matching can be computationally intensive, both input tables (base
#' and lookup) are first converted to data.table.
#'
#' Exact matches are performed using data.table syntax, which should be very
#' fast, even for large data sets.  Further information on the syntax used is
#' available in
#' [this StackOverflow post](https://stackoverflow.com/questions/73613758/get-indices-of-matches-with-a-column-in-a-second-data-table#73614750).
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
#' @param method one of "fuzzy" or "exact"
#' @param reason one of 'link new', 'edit nonlab', 'edit lab', 'add sequencing'
#'
#' @return The base data set is returned with new columns added in two formats:
#' (a) match report which indicates which criteria were met for each match and
#' (b) godata import-ready table which includes match IDs, date-time stamp and
#' 'sequenced' column, where relevant.
#'
#' @import lubridate
#' @import data.table
#' @import EmilMisc
#'
#' @examples
#' ########################################################
#' # 01. Link new lab results to existing cases in Go.Data:
#' ########################################################
#'
#' # Load example lab results:
#' data(new_lab_results)
#'
#' # Load example case data from Go.Data to match to:
#' data(case_lookup_table)
#'
#' # Match lab results to cases by names and dates of birth:
#' labresmatched <- match_cases(basedata = new_lab_results,
#'                              lookuptable = case_lookup_table,
#'                              epiwindow = 30,
#'                              matchcols = "names & dob",
#'                              firstnamecol = "firstname",
#'                              lastnamecol = "surname",
#'                              dobcol = "birthdate",
#'                              basedatecol = "sample_collection_date",
#'                              lookupdatecol = "dateOfReporting",
#'                              lookupmatchcol = "visualId",
#'                              method = "fuzzy",
#'                              reason = "link new")
#'
#' # Check match results:
#' labresmatched$match_report$match_id
#'
#' #################################################################
#' # 02. Link sequencing results to existing lab records in Go.Data:
#' #################################################################
#'
#' # Load example whole genome sequencing results:
#' data(updated_lab_results)
#'
#' # Load example lab records from Go.Data to match to:
#' data(lab_lookup_table)
#'
#' # Match sequencing results to lab records by names and dates of birth:
#' seqresmatched <- match_cases(basedata = updated_lab_results,
#'                              lookuptable = lab_lookup_table,
#'                              epiwindow = 30,
#'                              matchcols = "names & dob",
#'                              firstnamecol = "firstname",
#'                              lastnamecol = "surname",
#'                              dobcol = "birthdate",
#'                              basedatecol = "sample_date",
#'                              lookupdatecol = "godata_sampledate",
#'                              lookupmatchcol = "godata_system_lid",
#'                              method = "fuzzy",
#'                              reason = "add sequencing")
#'
#' # Check match results
#' seqresmatched$match_report$match_id
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
                        method = c("fuzzy",
                                   "exact"),
                        reason = c("link new",
                                   "edit nonlab",
                                   "edit lab",
                                   "add sequencing")){


  ###########################
  # -1. Helper functions:
  ###########################

  # Function to return multi-value intersects:
  intersect_all <- function(...){
    out = Reduce(intersect, list(...))
    out = list(out)
    return(out)
  }

  ###########################
  # 0. Prepare data sets:
  ###########################

  # Convert lookup and base data to data.table
  # to improve speed and memory consumption of matching process

  # Convert Go.Data lookup table to data.table:
  lookuptable =  as.data.table(lookuptable)

  # Convert base data to data.table:
  basedata = as.data.table(basedata)

  # Get original names of base data columns to remap to output:
  original_names = copy(names(basedata))


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

    # Create ID column lists to include:
    option1cols = c(firstnamecol, lastnamecol, dobcol, basedatecol)
    option2cols = c(firstnamecol, lastnamecol, agecol, basedatecol)
    option3cols = c(firstnamecol, lastnamecol, basedatecol)
    option4cols = c(docidcol, basedatecol)

    # Map base column names to facilitate rest of code:
    if(matchcols == "names & dob"){

      data.table::setnames(x = basedata,
                           old = option1cols,
                           new = c("firstName", "lastName", "dob", "basedate"))

    } else if(matchcols == "names & age"){

      data.table::setnames(x = basedata,
                           old = option2cols,
                           new = c("firstName", "lastName", "age_years", "basedate"))

    } else if(matchcols == "names"){

      data.table::setnames(x = basedata,
                           old = option3cols,
                           new = c("firstName", "lastName", "basedate"))

    } else if(matchcols == "doc ID"){

      data.table::setnames(x = basedata,
                           old = option4cols,
                           new = c("documents_number", "basedate"))

    }

  }

  # Record new mapped column names for base data so they can be reverted later:
  new_names = copy(names(basedata))

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


  if(method == "exact" & matchcols == "names & dob"){

    #####################################################################

    # Return exact matches on combination of firstName, lastName and dob:
    basedata[lookuptable, casematch := .I, on = .(firstName, lastName, dob)]

    # Now update with any matches where firstName and lastName are swapped:
    basedata[lookuptable, casematch := .I, on = .(firstName = lastName,
                                                  lastName = firstName,
                                                  dob)]

    # Next, check that specimen dates are within range of epiwindow:
    # Use abs function so we don't get negative numbers:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                      yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                      no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                    yes = lookuptable$type[unlist(casematch)],
                                    no = "no match"),
             by = 1:nrow(basedata)]

  } else if(method == "fuzzy" & matchcols == "names & dob"){

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
    basedata[, casematch := list(intersect_all(unlist(fn),
                                               unlist(ln),
                                               unlist(bd))),
             by = 1:nrow(basedata)]


    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                      yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                      no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                    yes = lookuptable$type[unlist(casematch)],
                                    no = "no match"),
             by = 1:nrow(basedata)]


  } else if(method == "exact" & matchcols == "names & age"){

    #####################################################################

    # Return exact matches on combination of firstName, lastName and age:
    basedata[lookuptable, casematch := .I, on = .(firstName, lastName, age_years)]

    # Now update with any matches where firstName and lastName are swapped:
    basedata[lookuptable, casematch := .I, on = .(firstName = lastName,
                                                  lastName = firstName,
                                                  age_years)]

    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                      yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                      no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                    yes = lookuptable$type[unlist(casematch)],
                                    no = "no match"),
             by = 1:nrow(basedata)]


  } else if(method == "fuzzy" & matchcols == "names & age"){

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
    basedata[, am := EmilMisc::mamatch(x = age_years,
                                           table = lookuptable$age_years,
                                           method = "dl",
                                           maxDist = 0,
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
    basedata[, casematch := list(intersect_all(unlist(fn),
                                               unlist(ln),
                                               unlist(am))),
             by = 1:nrow(basedata)]

    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                      yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                      no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                    yes = lookuptable$type[unlist(casematch)],
                                    no = "no match"),
             by = 1:nrow(basedata)]


  } else if(method == "exact" & matchcols == "names"){

    #####################################################################

    # Return exact matches on combination of firstName and lastName:
    basedata[lookuptable, casematch := .I, on = .(firstName, lastName)]

    # Now update with any matches where firstName and lastName are swapped:
    basedata[lookuptable, casematch := .I, on = .(firstName = lastName,
                                                  lastName = firstName)]

    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                      yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                      no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                    yes = lookuptable$type[unlist(casematch)],
                                    no = "no match"),
             by = 1:nrow(basedata)]


  } else if(method == "fuzzy" & matchcols == "names"){

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
    basedata[, casematch := list(intersect_all(unlist(fn),
                                               unlist(ln))),
             by = 1:nrow(basedata)]

    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                     yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                     no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                     yes = lookuptable$type[unlist(casematch)],
                                     no = "no match"),
             by = 1:nrow(basedata)]


  } else if(method == "exact" & matchcols == "doc ID"){

    ####################################################################

    # Return exact matches on document number:
    basedata[lookuptable, casematch := .I, on = .(documents_number)]

    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                      yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                      no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                    yes = lookuptable$type[unlist(casematch)],
                                    no = "no match"),
             by = 1:nrow(basedata)]


  } else if(method == "fuzzy" & matchcols == "doc ID"){

    ####################################################################

    # Check document number matches by Damereau-Levenshtein distance (max of 1):
    basedata[, did := EmilMisc::mamatch(x = documents_number,
                                       table = lookuptable$documents_number,
                                       method = "lv",
                                       maxDist = 1,
                                       maxmatch = nrow(lookuptable),
                                       returnAs = "list")]


    # Return index that matches in all three columns:
    basedata[, casematch := fifelse(test = lengths(did) == 1,
                                    yes = lapply(did, unlist),
                                    no = list(NA)),
            by = 1:nrow(basedata)]

    # Next, check that specimen dates are within range of epiwindow:
    basedata[, epidatediff := ifelse(test = lengths(casematch) == 1,
                                      yes = abs(basedate - lookuptable$lookupdate[unlist(casematch)]),
                                      no = NA),
             by = 1:nrow(basedata)]

    # Compare epidatediff with epiwindow:
    basedata[, epicheck := fifelse(test = !is.na(epidatediff) &
                                     epidatediff <= epiwindow,
                                   yes = TRUE,
                                   no = FALSE)]

    # Finally add the Go.Data visual case ID for full matches:
    basedata[, match_id := ifelse(test = lengths(casematch) == 1 &
                                    epicheck == TRUE,
                                  yes = lookuptable$match_id[unlist(casematch)],
                                  no = "no match"),
             by = 1:nrow(basedata)]

    # Add column indicating whether lab result is for a case or a contact:
    basedata[, match_type := ifelse(test = match_id != "no match",
                                    yes = lookuptable$type[unlist(casematch)],
                                    no = "no match"),
             by = 1:nrow(basedata)]

  }


  #####################
  # 3. Prepare output:
  ####################

  ####################################################################
  # Add additional columns to output for lab data:

  # Add updated at column if matches are for editing existing records:
  if(reason != "link new"){

    # Add Updated at column
    basedata[match_id != "no match",
             godata_updatedat := mongify_date(Sys.time())]

  }

  # Add other columns for matched cases if editing lab data:
  if(reason %in% c("edit lab", "add sequencing")){

    # Add Go.Data visual case ID column:
    basedata[match_id != "no match",
             godata_caseid := lookuptable$godata_caseid[unlist(casematch)]]

    # Add Go.Data sample date (mandatory for bulk importing lab data):
    basedata[match_id != "no match",
             godata_sampledate := lookuptable$lookupdate[unlist(casematch)]]

  }

  # If adding sequencing results:
  if(reason == "add sequencing"){

    basedata[match_id != "no match", sequenced := TRUE]

  }


  #######################################################################
  # Create final outputs:

  # List of new columns that may have been added to basedata during matching:
  if(matchcols == "names & dob" & method == "fuzzy"){

    # Short col names used in code:
    reportcolsin = c("fn",
                     "ln",
                     "bd",
                     "casematch",
                     "epidatediff",
                     "epicheck",
                     "match_id",
                     "match_type")

    # More user-friendly col names for output:
    reportcolsout = c("match_firstname",
                      "match_lastname",
                      "match_birthdate",
                      "match_index",
                      "epidatediff",
                      "match_epiwindow",
                      "match_id",
                      "match_type")


  } else if(matchcols == "names & age" & method == "fuzzy"){

    # Short col names used in code:
    reportcolsin = c("fn",
                     "ln",
                     "am",
                     "casematch",
                     "epidatediff",
                     "epicheck",
                     "match_id",
                     "match_type")

    # More user-friendly col names for output:
    reportcolsout = c("match_firstname",
                      "match_lastname",
                      "match_ageyears",
                      "match_index",
                      "epidatediff",
                      "match_epiwindow",
                      "match_id",
                      "match_type")

  } else if(matchcols == "names" & method == "fuzzy"){

    # Short col names used in code:
    reportcolsin = c("fn",
                     "ln",
                     "casematch",
                     "epidatediff",
                     "epicheck",
                     "match_id",
                     "match_type")

    # More user-friendly col names for output:
    reportcolsout = c("match_firstname",
                      "match_lastname",
                      "match_index",
                      "epidatediff",
                      "match_epiwindow",
                      "match_id",
                      "match_type")

  } else if(matchcols == "doc ID" & method == "fuzzy"){

    # Short col names used in code:
    reportcolsin = c("did",
                     "casematch",
                     "epidatediff",
                     "epicheck",
                     "match_id",
                     "match_type")

    # More user friendly col names for output:
    reportcolsout = c("match_documentid",
                      "match_index",
                      "epidatediff",
                      "match_epiwindow",
                      "match_id",
                      "match_type")

  } else if(method == "exact"){

    reportcolsin = c("casematch",
                     "epidatediff",
                     "epicheck",
                     "match_id",
                     "match_type")

    reportcolsout = c("match_index",
                      "epidatediff",
                      "match_epiwindow",
                      "match_id",
                      "match_type")
  }


  # Create match report:
  match_report = basedata

  # Update match_id column to no match if casematch is not unique:
  match_report[, match_id := ifelse(test = lengths(casematch) > 1,
                                    yes = "no match",
                                    no = match_id)]

  # Update match_type column to no match if casematch is not unique:
  match_report[, match_type := ifelse(test = lengths(casematch) > 1,
                                      yes = "no match",
                                      no = match_type)]

  # Extract the names of list columns:
  lnames <- names(match_report)[which(sapply(match_report, is.list))]

  # If there are list columns:
  if(length(lnames) != 0){

    # Convert all values in list columns to '>5 matches' if length > 5:
    match_report[, (lnames) := lapply(.SD, FUN = function(x)
      ifelse(test = sapply(x, function(x) length(x) > 5),
             yes = ">5 matches",
             no = x)),
      .SDcols = lnames]

    # If needed, convert the final columns from lists to character vectors:
    match_report[, (lnames) := lapply(.SD, FUN = function(x)
      sapply(x, paste0, collapse = ",")),
      .SDcols = lnames]

  }

  # Revert columns from input basedata data back to their original names:
  data.table::setnames(x = match_report,
                       old = new_names,
                       new = original_names)


  # Update match report column names to user friendly ones:
  data.table::setnames(x = match_report,
                       old = reportcolsin,
                       new = reportcolsout)

  # Ensure that match_index has NAs for blank values:
  match_report[, names(match_report) := lapply(.SD,
                                               function(x)
                                                 ifelse(test = x %in% c("NA", ""),
                                                        yes = NA,
                                                        no = x)),
               .SDcols = names(match_report)]

  # Get list of added columns for lab updates to keep in export:
  godatacols = names(match_report)[!names(match_report) %in% original_names &
                                     !names(match_report) %in% reportcolsout]

  # Created Go.Data import-ready output:
  if(length(godatacols) > 0){

    matched_data = subset(match_report,
                          match_id != "no match",
                          select = c(original_names,
                                     "match_id",
                                     godatacols,
                                     "match_type"))

  } else {

    matched_data = subset(match_report,
                          match_id != "no match",
                          select = c(original_names,
                                     "match_id",
                                     "match_type"))

  }

  # Create customised short report for shiny app:
  if(matchcols == "names & dob"){

    short_report = subset(x = match_report,
                          select = c(option1cols, reportcolsout))

  } else if(matchcols == "names & age"){

    short_report = subset(x = match_report,
                          select = c(option2cols, reportcolsout))

  } else if(matchcols == "names"){

    short_report = subset(x = match_report,
                          select = c(option3cols, reportcolsout))

  } else if(matchcols == "doc ID"){

    short_report = subset(x = match_report,
                          select = c(option4cols, reportcolsout))

  }

  # Create match status column:
  match_report[, `Match status` := ifelse(test = match_id != "no match",
                                        yes = "matched",
                                        no = "not matched"),
               by = 1:nrow(match_report)]

  # Create unique row IDs:
  if(matchcols == "names & dob"){

    match_report[, index := .GRP, by = option1cols]

  } else if(matchcols == "names & age"){

    match_report[, index := .GRP, by = option2cols]

  } else if(matchcols == "names"){

    match_report[, index := .GRP, by = option3cols]

  } else if(matchcols == "doc ID"){

    match_report[, index := .GRP, by = option4cols]

  }

  # Create summary table of number of matches:
  match_summary = data.table::dcast(data = match_report,
                                    formula = `Match status` ~ .,
                                    fun.aggregate = length,
                                    value.var = "index")

  # Update name of result column:
  data.table::setnames(x = match_summary,
                       old = ".",
                       new = "Lab records",
                       skip_absent = TRUE)


  # Create list object to export:
  gdmatches = list(match_report = match_report,
                   match_summary = match_summary,
                   short_report = short_report,
                   matched_data = matched_data)

  # Return updated lab data with Go.Data visual case IDs added for matches:
  return(gdmatches)

}

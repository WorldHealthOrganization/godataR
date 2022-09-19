#' Wrapper function to link lab data to Go.Data cases and lab records
#'
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#'
#' @description
#' This function is a wrapper function that imports lab data, matches it to
#' existing Go.Data case or lab records by patient identifier columns and
#' returns the updated lab records with the match IDs appended.  It is intented
#' to help Go.Data users link lab records to Go.Data case or lab record IDs by
#' exact or fuzzy matching on patient demographics, as this facility is not yet
#' available within Go.Data. The exported output file may be directly imported
#' to Go.Data using the bulk import facility.  Columns within the file can be
#' mapped to Go.Data fields in the normal way;
#' see [Go.Data user manual chapter nine](https://apps.who.int/iris/bitstream/handle/10665/332255/WHO-2019-nCoV-Go.data_manual-2020.2-eng.pdf?sequence=1&isAllowed=y))
#'
#' @seealso
#' The wrapper uses the following functions from this (`godataR`) package:
#'
#'   + [get_active_outbreak()]
#'   + [get_access_token()]
#'   + [get_date_range()]
#'   + [mongify_date()]
#'   + [get_cases_epiwindow()]
#'   + [get_labresults_epiwindow()]
#'   + [match_cases()]
#'
#' These functions can be tested with example data included in this package:
#'
#'   + [new_lab_results]
#'   + [case_lookup_table]
#'   + [updated_lab_results]
#'   + [lab_lookup_table]
#'
#' @md
#'
#' @param url The URL or web address for your Go.Data instance
#' @param username Your Go.Data username (email address used for logging in)
#' @param password Your Go.Data password
#' @param outbreak Default is your active outbreak; else enter the outbreak ID
#' @param reason Match reason: one of "link new", "edit lab" or "add sequencing"
#' @param datequery Default is "epibuffer"; else choose from "date range",
#' "epiwindow", or "sampledates"
#' @param daterangeformat One of "ymd", "dmy" or "mdy" (y=year, m=month, d=day)
#' @param epiwindow limit in days between dates from labdata and lookuptable
#' @param method Method to match on; one of "fuzzy" or "exact"
#' @param matchcols One of "names & dob", "names & age", "names", or "doc ID"
#' @param labdata Lab data for matching; either data.frame or data.table
#' @param basedatecol Name of lab column containing sample dates (mandatory)
#' @param firstnamecol Name of lab column containing first names, if needed
#' @param lastnamecol Name of lab column containing last names, if needed
#' @param dobcol Name of lab column containing birthdates, if needed
#' @param agecol Name of lab column containing age in years, if needed
#' @param docidcol Name of lab column containing document ID, if needed
#'
#' @return matched lab results in `match_report` and `matched_data`
#'
#' @examples
#' \dontrun{
#' # Set file path of lab data to import:
#' labdata_path <- here::here("data", "Lab_results.xlsx")
#'
#' # Fuzzy match new lab results to Go.Data cases using names & date of birth:
#' labmatches <- lab2godata_wrapper(url = url,
#'                                  username = username,
#'                                  password = password,
#'                                  outbreak = "active",
#'                                  reason = "link new",
#'                                  datequery = "epibuffer",
#'                                  daterangeformat = "ymd",
#'                                  epiwindow = 30,
#'                                  method = "fuzzy",
#'                                  matchcols = "names & dob",
#'                                  labdata = mylabdata,
#'                                  basedatecol = "sample_collection_date",
#'                                  firstnamecol = "firstname",
#'                                  lastnamecol = "surname",
#'                                  dobcol = "birthdate")
#' }
#'
#' @export
lab2godata_wrapper <- function(# Go.Data user credentials:
                               url = "http://localhost:3000/",
                               username,
                               password,
                               outbreak = "active",
                               # Define purpose of output:
                               reason = c("link new",
                                          "edit lab",
                                          "add sequencing"),
                               # Define date range for match data:
                               datequery = "epibuffer",
                               daterangeformat = c("ymd", "dmy", "mdy"),
                               epiwindow,
                               # Define match method:
                               method = c("fuzzy",
                                          "exact"),
                               # Combination of columns to match on:
                               matchcols = c("names & dob",
                                             "names & age",
                                             "names",
                                             "doc ID"),
                               # lab data to match:
                               labdata,
                               basedatecol,
                               firstnamecol = NULL,
                               lastnamecol = NULL,
                               dobcol = NULL,
                               agecol = NULL,
                               docidcol = NULL){

  ########################################################################
  # 01. Get date range for base data:

  daterange = get_date_range(dates = labdata[, basedatecol])

  ########################################################################
  # 02. Import the Go.Data data you want to retrieve matches from:

  if(reason == "link new" & datequery == "date range"){

    # Import case data from Go.Data within date range:
    caselookup = get_cases_epiwindow(url = url,
                                     username = username,
                                     password = password,
                                     outbreak = "active",
                                     cols2return = "identifiers",
                                     datequery = datequery,
                                     daterangeformat = daterangeformat,
                                     mindate = daterange$mindate,
                                     maxdate = daterange$maxdate)

  } else if(reason == "link new" & datequery == "epiwindow"){

    # Import case data from Go.Data within epiwindow:
    caselookup = get_cases_epiwindow(url = url,
                                     username = username,
                                     password = password,
                                     outbreak = "active",
                                     cols2return = "identifiers",
                                     datequery = datequery,
                                     epiwindow = epiwindow)

  } else if(reason == "link new" & datequery == "epibuffer"){

    caselookup = get_cases_epiwindow(url = url,
                                     username = username,
                                     password = password,
                                     outbreak = "active",
                                     cols2return = "identifiers",
                                     datequery = datequery,
                                     daterangeformat = daterangeformat,
                                     mindate = daterange$mindate,
                                     maxdate = daterange$maxdate,
                                     epiwindow = epiwindow)

  } else if(reason %in% c("edit lab", "add sequencing") &
            datequery == "date range"){

    lablookup = get_labresults_epiwindow(url = url,
                                         username = username,
                                         password = password,
                                         outbreak = "active",
                                         cols2return = "identifiers",
                                         datequery = datequery,
                                         daterangeformat = daterangeformat,
                                         mindate = daterange$mindate,
                                         maxdate = daterange$maxdate)

  } else if(reason %in% c("edit lab", "add sequencing") &
            datequery == "epiwindow"){

    lablookup = get_labresults_epiwindow(url = url,
                                         username = username,
                                         password = password,
                                         outbreak = "active",
                                         cols2return = "identifiers",
                                         datequery = datequery,
                                         epiwindow = epiwindow)

  } else if(reason %in% c("edit lab", "add sequencing") &
            datequery == "sampledates"){

    lablookup = get_labresults_epiwindow(url = url,
                                         username = username,
                                         password = password,
                                         outbreak = "active",
                                         cols2return = "identifiers",
                                         datequery = datequery,
                                         daterangeformat = daterangeformat,
                                         sampledates = labdata[, basedatecol])

  }

  ########################################################################
  # 03. Match data with Go.Data case or lab records:


  if(reason == "link new"){

    # Derive column names from output of get_cases_epiwindow():
    lookupdatecol = "dateOfReporting"
    lookupmatchcol = "visualId"

  } else {

    # Otherwise derive column names from output of get_labresults_epiwindow():
    lookupdatecol = "godata_sampledate"
    lookupmatchcol = "godata_system_lid"

  }

  if(reason == "link new" & matchcols == "names & dob"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = caselookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             firstnamecol = firstnamecol,
                             lastnamecol = lastnamecol,
                             dobcol = dobcol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  } else if(reason == "link new" & matchcols == "names & age"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = caselookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             firstnamecol = firstnamecol,
                             lastnamecol = lastnamecol,
                             agecol = agecol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  } else if(reason == "link new" & matchcols == "names"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = caselookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             firstnamecol = firstnamecol,
                             lastnamecol = lastnamecol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  } else if(reason == "link new" & matchcols == "doc ID"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = caselookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             docidcol = docidcol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  } else if(reason %in% c("edit lab", "add sequencing") &
            matchcols == "names & dob"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = lablookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             firstnamecol = firstnamecol,
                             lastnamecol = lastnamecol,
                             dobcol = dobcol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  } else if(reason %in% c("edit lab", "add sequencing") &
            matchcols == "names & age"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = lablookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             firstnamecol = firstnamecol,
                             lastnamecol = lastnamecol,
                             agecol = agecol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  } else if(reason %in% c("edit lab", "add sequencing") &
            matchcols == "names"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = lablookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             firstnamecol = firstnamecol,
                             lastnamecol = lastnamecol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  } else if(reason %in% c("edit lab", "add sequencing") &
            matchcols == "doc ID"){

    labmatched = match_cases(basedata = labdata,
                             lookuptable = lablookup,
                             epiwindow = epiwindow,
                             matchcols = matchcols,
                             docidcol = docidcol,
                             basedatecol = basedatecol,
                             lookupdatecol = lookupdatecol,
                             lookupmatchcol = lookupmatchcol,
                             method = method,
                             reason = reason)

  }


  ########################################################################
  # 04. Return match report and matched data to R environment:

  # Return outputs to R environment:
  return(labmatched)

}

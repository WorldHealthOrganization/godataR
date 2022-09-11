#' Export new lab results in format ready for bulk import to Go.Data 
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function prepares case-matched lab results for bulk import to Go.Data.
#' Two files are exported, one containing the match report columns and one 
#' clean file ready for importing to Go.Data with the mandatory identifying 
#' columns appended.  Usually this will be the output of `match_cases()` 
#' exported to a Microsoft Excel (xlsx) worksheet.
#' 
#' @md
#'  
#' @param original data.frame of original lab data before case matching
#' @param forexport  data.frame of Go.Data case ID-matched lab results
#' 
#' @return 
#' Returns a list of two exports (match report and export file) and exports 
#' the files to .xlsx  
#' 
#' @import lubridate
#' @import rio
#' @import data.table
#' 
#' @examples 
#' \dontrun{
#' # Export lab results:
#' exported_labres <- export_labresults_new(original = labdata, 
#'                                          forexport = labmatched)
#' }
#' @export
export_labresults_new <- function(original, forexport){
  
  # List columns created by match_cases to remove from export file:
  cols4matchreport = c("fn", 
                       "ln", 
                       "bd", 
                       "am", 
                       "did", 
                       "casematch", 
                       "epidatediff", 
                       "epicheck", 
                       "match_id")
  
  # List match report columns included in data to export:
  matchcols = names(forexport)[names(forexport) %in% cols4matchreport]
  
  # List updated column names from match report to revert to original:
  newcolnames = names(forexport)[!names(forexport) %in% cols4matchreport] 
  
  # Get original column names:
  oldcolnames = names(original)
  
  # Create first data set with match report:
  matchreport = forexport
  
  # Revert to original column names:
  data.table::setnames(x = matchreport,
                       old = newcolnames, 
                       new = oldcolnames)
  
  # Create export data:
  exportlab = subset(matchreport, select = c(oldcolnames, "match_id"))
  
  # Update name of match column:
  data.table::setnames(x = exportlab, 
                       old = "match_id", 
                       new = "godata_case_id")
  
  
  # Export the match report:
  rio::export(x = matchreport, 
              file = paste0("Go.Data lab results case match report_", 
                           Sys.Date(), 
                           ".xlsx"))
  
  # Export the export file:
  rio::export(x = exportlab, 
              file = paste0("Go.Data case-matched lab results_", 
                           Sys.Date(), 
                           ".xlsx"))
  
  # Create a list of the two export objects:
  labout = list(matchreport = matchreport, labexport = exportlab)
  
  # Return the two export objects:
  return(labout)
  
}
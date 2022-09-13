#' Line list of sequencing results to update existing Go.Data lab records
#'
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#'
#' Example line list of whole genome sequencing results that need to be
#' bulk-imported to Go.Data in order to update existing lab records that only
#' have the primary diagnostic test result. This data can be used as base data
#' with `match_cases()` to demonstrate how to link sequencing data with existing
#' lab results in Go.Data and update the lab records.  Note that this is not
#' real data; it has been created for the purpose of demonstration only.
#' @md
#'
#' @docType data
#'
#' @usage data(updated_lab_results)
#'
#' @format An object of class `data.frame` and `data.table`
#'
#' @keywords datasets
#'
#' @examples
#' # Load data
#' data(updated_lab_results)
#'
#' # Look at head of data set:
#' head(updated_lab_results)
"updated_lab_results"

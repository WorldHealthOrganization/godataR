#' Go.Data exported line list of lab results with identifying columns
#'
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#'
#' Example line list of lab results exported from Go.Data with patient and lab
#' record identifying columns. This data can be used as a reference (look-up)
#' table with `match_cases()` to demonstrate how to link new information with
#' existing lab results and update the lab records in Go.Data.  Note that this
#' is not real data; it has been created for the purpose of demonstration only.
#' @md
#'
#' @docType data
#'
#' @usage data(lab_lookup_table)
#'
#' @format An object of class `data.frame` and `data.table`
#'
#' @keywords datasets
#'
#' @examples
#' # Load data
#' data(lab_lookup_table)
#'
#' # Look at head of data set:
#' head(lab_lookup_table)
"lab_lookup_table"

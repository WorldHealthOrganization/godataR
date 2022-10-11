#' Convert NULL slots in a data.table to NA
#'
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#'
#' @description
#' This function searches for NULL elements in a vector containing lists and
#' converts them to the appropriate type of NA. It is useful for transforming
#' columns containing lists of NULL values from Go.Data, as these columns
#' cannot be un-nested until the NULL values are converted to NA.
#'
#' @param dtcol vector containing lists with NULL slots to convert
#'
#' @returns the vector with NULLs converted to NAs
#'
#' @examples
#'  \dontrun{
#'  # Loop through a data.table to convert all NULLs to NAs:
#'  cases[, names(cases) := lapply(.SD, null2na)]
#'  }
#' @export
null2na <- function(dtcol){
  nowna = replace(dtcol, lengths(dtcol) == 0L, NA_real_)
  return(nowna)
}

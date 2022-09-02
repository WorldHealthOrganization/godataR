#' Get minimum and maximum date for a vector of dates 
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function will attempt to auto-detect the date format using lubridate, 
#' then determine the minimum and maximum for the submitted vector of dates.  
#' It is intended as a helper function for selecting date ranges and adding
#' them to queries.
#' 
#' @md
#'  
#' @param dates character vector of dates to extract the date range from
#' 
#' @return 
#' Returns a list object with two values (minimum and maximum date)  
#' 
#' @import lubridate
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
get_date_range <- function(dates){
  
  # Check if the input variable is already in date format:
  if(any(class(dates) %in% c("Date", "POSIXt"))){
    
    # If yes, no need to format:
    dates2range = dates
    
  } else {
    
    # If no, convert to date format with lubridate:
    dates2range = lubridate::parse_date_time(x = dates, 
                                             orders = c("ymd", 
                                                        "dmy", 
                                                        "mdy"))
  }
  
  # Get the minimum date:
  mindate = min(dates2range, na.rm = TRUE)
  
  # Get the maximum date:
  maxdate = max(dates2range, na.rm = TRUE)
  
  # Compile results:
  daterange = list(mindate = mindate, maxdate = maxdate)
  
  # Return the results:
  return(daterange)
  
}
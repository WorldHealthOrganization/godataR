#' Format dates for importing to Go.Data
#'
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#'
#' @description
#' This function uses a date parser from the lubridate package to convert dates
#' to the required format for importing dates into the Go.Data mongodb database,
#' which is **YYYY-MM-DDThh:mm:ss.000z** where:
#'   + Y = year
#'   + M = month
#'   + D = day
#'   + h = hours
#'   + m = minutes
#'   + s = seconds
#'   + Remaining characters are milliseconds with mongodb suffix (z)
#'
#' Input dates must already have 2 digits for the day and month and 4 digits
#' for year (names or abbreviations for months are not accepted), however the
#' day, month and year can be in any order and with any separator.
#'
#' The user can identify the format of the date column they wish to convert as
#' one of:
#'   + undefined (default) = tries ymd first, then dmy, then mdy
#'   + dmy = date first, then month, then year e.g. 25-09-2022
#'   + mdy = month first, then date, then year e.g. 09-25-2022
#'   + ymd = year first, then month, then date (ISO standard) e.g. 2022-09-25
#'
#' Identifying the order of the date elements will help to distinguish between,
#' for example 08 May 2022 (08/05/2022) and 05 August 2022 (05/08/2022) or any
#' other date combinations where the month and date are not easily to
#' differentiate based on the size of the number.  Note that only one date
#' element order can be selected (i.e. all dates in the column to be converted
#' must be in the same format).
#'
#' Dates with and without a time-stamp are both accepted. Dates which do include
#' a time stamp must include hours, minutes and seconds in the format **HH:mm:ss**
#'
#' @md
#'
#' @param dates Vector of dates to convert. Dates must have 2-digit months and 4-digit years. For example, "05/05/2022
#' @param dateformat Indicate format of dates to convert ("dmy", "mdy" or "ymd")
#'
#' @return
#' Returns character vector of dates converted to Go.Data / mongodb format
#'
#' @import lubridate
#'
#' @examples
#' # Create dummy dataframe with dates to convert:
#' mydf <- data.frame(id = c(1,2), mydates = c("08/01/2022", "08/31/2022"))
#'
#' # Add a new column with the converted dates:
#' mydf$converted <- mongify_date(dates = mydf$mydates, dateformat = "mdy")
#'
#' # View the result:
#' mydf
#'
#' @export
mongify_date <- function(dates,
                         dateformat = "undefined"){

  # Define format of input dates:
  if(dateformat == "undefined"){
    format2search = c("ymd_HMS", "dmy_HMS", "mdy_HMS")}
  if(dateformat == "ymd"){format2search = "ymd_HMS"}
  if(dateformat == "dmy"){format2search = "dmy_HMS"}
  if(dateformat == "mdy"){format2search = "mdy_HMS"}

  # Make sure dates are strings:
  dates = as.character(dates)

  # Check if dates already have a time-stamp, if not add time:
  dflong = ifelse(nchar(dates) == 10,
                  paste0(dates, " 00:00:00"),
                  dates)

  # Convert date-times to posixct format:
  dfpct = lubridate::parse_date_time(x = dflong,
                                     orders = format2search,
                                     tz = "UTC")

  # Add the Godata / Mongodb specific format ending for date-time:
  dfmongo = format(x = dfpct, format = "%Y-%m-%dT%H:%M:%S.000Z")

  # Return the formatted dates:
  return(dfmongo)

}

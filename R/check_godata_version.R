#' Check if installed verison of Go.Data is 2.38.1 or later
#'
#' @param url Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
#'
#' @return boolean
#' @examples
#' url <- "https://MyGoDataServer.com/"
#' check_godata_version(url=url)
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
check_godata_version <- function(url=url) {

  # Get Current Version of Go.Data
  gd.version <- get_godata_version(url=url)

  # Convert string to vector of 3 numbers
  gd.version <- str_split(gd.version, "[.]") %>%
    unlist() %>%
    as.numeric()

  # Check if 2.38.1 or later
  # Should be TRUE if it is version 2.38.1 or later &
  # FALSE if version 2.38.0 or earlier
  if (gd.version[1] < 2) {
    after.2.38.1 <- FALSE
  } else if (gd.version[1]==2 & gd.version[2] < 38) {
    after.2.38.1 <- FALSE
  } else if (gd.version[1]==2 & gd.version[2]==38 & gd.version[3]<1) {
    after.2.38.1 <- FALSE
  } else if (gd.version[1]==2 & gd.version[2]==38 & gd.version[3]>=1) {
    after.2.38.1 <- TRUE
  } else if (gd.version[1]==2 & gd.version[2]>38) {
    after.2.38.1 <- TRUE
  } else if (gd.version[1]>2) {
    after.2.38.1 <- TRUE
  }

  return(after.2.38.1)

}

#' Convert Nested Columns to Data Frames
#'
#' A helper function to go through datasets and convert any nested columns to data frames.
#'
#' This function works on all versions of Go.Data. There
#' are two methods for downloading the data:
#'
#'
#' @param df The input data frame that contains nested columns.

#' @return
#' Returns a data frame. All nested columns will be coerced to data frames.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' cases <- get_cases(url=url,
#'                    username=username,
#'                    password=password,
#'                    outbreak_id=outbreak_id)
#'
#' cases1 <- nested_cols_to_df(cases)
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck

nested_cols_to_df <- function(df) {

  df_1 <- lapply(df, function(x){

    #Any time a column has a length of >1, then it converts that column to a data frame.
    if (length(x) > 1) {
      return(as.data.frame(x))
    } else {
      return(x)
    }
  })

  return(df_1)
}

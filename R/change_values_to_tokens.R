#' Replace column values with Go.Data tokens
#'
#' A function to replace values in a field with tokens from reference data
#'
#' A useful function when preparing data to be imported into Go.Data.
#'
#' @param data A data frame to be imported into Go.Data
#' @param column_name The name of the column to be replaced with tokens.
#' @param language_tokens A language token object by using the `get_language_tokens()` function.
#'
#' @return
#' A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#' outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"
#'
#' language_tokens <- get_language_tokens(url=url,
#'                                        username=username,
#'                                        password=password,
#'                                        language="english_us")
#'
#' #Example data frame to be imported into Go.Data
#' x <- data.frame(visualId=c("CASE-001","CASE-002"),
#'                 gender=c("Male","Female"))
#' x_new <- change_values_to_tokens(data=x, column_name="gender", language_tokens=language_tokens)
#' }

change_values_to_tokens <- function(data,column_name,language_tokens=language_tokens) {

  df <- data

  #Column Names According to Reference Data
  if (toupper(column_name)=="CLASSIFICATION") {
    column_name_ref <- "CASE_CLASSIFICATION"
  } else {
    column_name_ref <- toupper(column_name)
  }

  #Get Reference Data from the Language Tokens
  ref_data <- language_tokens %>%
    select(tokens) %>%
    filter(grepl("LNG_REFERENCE_DATA_CATEGORY_",tokens$token)) %>%
    unnest_wider(tokens) %>%
    filter(grepl(column_name_ref,token))

  #Go Row by Row in the specified column
  #Find the token in the reference data
  #substitute token value for the data value
  for (i in 1:nrow(data)) {

    value_i <- data[i,column_name]
    if (!is.na(value_i) & value_i!="") {

      #Find this column/value in the reference data
      value_token <- ref_data %>%
        filter(translation==value_i) %>%
        pluck("token")


      #replace value in data frame with lookup value from reference data
      df[i,column_name] <- value_token
    }
  }

  return(df)
}

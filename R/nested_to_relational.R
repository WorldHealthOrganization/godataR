#' Helper function to deal with nested values
#'
#' A housekeeping function to convert a single nested and difficult dataset to multiple relational and flat tables
#'
#' @param df a single dataframe with nested columns

#' @return
#' Returns multiple data frames.
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
#' }
#'
#' cases_flat <- nested_to_relational(cases)
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import tibble
#'
#'
nested_to_relational <- function(df) {

  number_of_rows <- nrow(df)

  df <- df %>% select(!contains("questionnaireAnswers.")) #Exclude Questionnaire Fields

  df_flat <- df %>%
    select(!where(is.list))

  df_nested <- df %>%
    select(id, where(is.list))

  names_nested <- names(df_nested)

  df_group <- list(flat=df_flat, nested=df_nested)

  # for (x in names_nested) {
  #   assign(x, df_nested[x] %>% as_tibble())
  #   append(df_group, x)
  # }

  return(df_group)

}



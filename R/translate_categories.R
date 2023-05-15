#' Translates cateogories with API labels to more readable forms using the
#' translation specified in the output of `get_language_tokens()`.
#'
#' @param data A data frame (or data frame extension)
#' @param language_tokens A data frame (or data frame extension) containing
#' the translations. Output from `get_language_tokens()`
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' cases <- get_cases(
#'   url = url,
#'   username = username,
#'   password = password,
#'   outbreak_id = outbreak_id
#' )
#'
#' language_tokens <- get_language_tokens(
#'   url = url,
#'   username = username,
#'   password = password,
#'   language = "english_us"
#' )
#'
#' translate_categories(
#'   data = cases,
#'   language_tokens = language_tokens
#' )
#' }
translate_categories <- function(data, language_tokens) {

  stopifnot(
    "data must be tabular data" =
      is.data.frame(data),
    "language_tokens must be tabular data" =
      is.data.frame(language_tokens)
  )

  if (isFALSE(any_tokens(data, language_tokens))) {
    return(data)
  }

  data <- translate_token(data = data, language_tokens = language_tokens)

  data <- tibble::as_tibble(data)

  return(data)
}

#' Translates tokens given a specified translation.
#'
#' @description This function does all the translation for
#' `translate_categories`.
#'
#' @inheritParams translate_categories
#'
#' @return A tibble
#' @keywords internal
translate_token <- function(data, language_tokens) {
  if (is.list(data)) {
    for (i in seq_along(data)) {
      if (any_tokens(data[[i]], language_tokens)) {
        data[[i]] <- translate_token(
          data = data[[i]],
          language_tokens = language_tokens
        )
      }
    }
  } else {
    token_index <- lapply(
      data,
      function(x) which(language_tokens$tokens$token %in% x)
    )
    token_index <- unlist(
      lapply(
        token_index,
        function(x) if (length(x) == 0) NA else x
      )
    )
    data <- language_tokens$tokens$translation[token_index]
  }
  return(data)
}

#' Checks if there are any recognized tokens in the data provided
#'
#' @inheritParams translate_categories
#'
#' @return Boolean logical (TRUE or FALSE)
#' @keywords internal
any_tokens <- function(data, language_tokens) {
  any(unname(unlist(data)) %in% language_tokens$tokens$token)
}

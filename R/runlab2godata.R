#' Function to launch the lab2godata shiny app
#'
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#'
#' @description
#' This function will launch the lab2godata shiny app, which allows users to
#' upload lab data and select the required parameters for the
#' `lab2godata_wrapper()` function via a user-friendly interface provided by
#' the app.  In this way, users with no knowledge of R can run the function.
#'
#' @seealso
#'  See [lab2godata_wrapper()] for further information about the function.
#'
#' @md
#'
#' @import shiny
#' @import rio
#'
#' @examples
#' \dontrun{
#' Launch the lab2godata app:
#' runlab2godata()
#' }
#'
#' @export
runlab2godata <- function() {

  appDir = system.file("app", "lab2godata.R", package = "godataR")

  if (appDir == "") {

    stop("Could not find shiny app directory. Try re-installing `godataR`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cases_questionnaire.R
\name{get_cases_questionnaire}
\alias{get_cases_questionnaire}
\title{Download cases from Go.Data and returns questionnaire fields}
\usage{
get_cases_questionnaire(url, username, password, outbreak_id, wait = 2)
}
\arguments{
\item{url}{Insert the base URL for your instance of Go.Data here.
Don't forget the forward slash "/" at end!}

\item{username}{The email address for your Go.Data login.}

\item{password}{The password for your Go.Data login}

\item{outbreak_id}{The id number for the outbreak for which you want to
download data.}

\item{wait}{If \code{method = "export"}, then \code{wait} is the number of seconds to
wait in between iterations of checking the status of the export.}
}
\value{
Returns a \code{tibble}.
}
\description{
A function that retrieves the questionnaire fields from case data for a
specific \code{outbreak_id}.
}
\details{
Unlike \code{\link[=get_cases]{get_cases()}} this function only uses the \code{\link[=export_downloader]{export_downloader()}},
and not the \code{\link[=batch_downloader]{batch_downloader()}}. Therefore, this function will only work
on Go.Data versions 2.38.1 or newer. This method relies on the GET
outbreak/{id}/cases/export API endpoint. An export request is submitted to
the server, and then when the export is ready, it will be downloaded.

This function fixes the file return type to \code{"csv"}.
}
\examples{
\dontrun{
url <- "https://MyGoDataServer.com/"
username <- "myemail@email.com"
password <- "mypassword"
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"

cases <- get_cases_questionnaire(
  url = url,
  username = username,
  password = password,
  outbreak_id = outbreak_id
)
}
}

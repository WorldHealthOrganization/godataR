% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_active_outbreak.R
\name{get_active_outbreak}
\alias{get_active_outbreak}
\title{Get the currently active outbreak id number}
\usage{
get_active_outbreak(url, username, password)
}
\arguments{
\item{url}{Insert the base URL for your instance of Go.Data here. Don't
forget the forward slash "/" at end!}

\item{username}{The email address for your Go.Data login.}

\item{password}{The password for your Go.Data login.}
}
\value{
Returns a string with the Outbreak ID number of the user's active outbreak.
}
\description{
A function to retrieve the active outbreak
ID number for the provided username. Each
Go.Data user can have 1 and only 1 active
outbreak at a given time. This is a
housekeeping function used in many of the
other \code{godataR} functions.
}
\examples{
\dontrun{
url <- "https://MyGoDataServer.com/"
username <- "myemail@email.com"
password <- "mypassword"

active_outbreak_id <- get_active_outbreak(
  url = url,
  username = username,
  password = password
)
}
}

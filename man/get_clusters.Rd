% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_clusters.R
\name{get_clusters}
\alias{get_clusters}
\title{Download clusters from Go.Data (version agnostic)}
\usage{
get_clusters(url, username, password, outbreak_id, batch_size = 50000)
}
\arguments{
\item{url}{Insert the base URL for your instance of Go.Data here. Don't
forget the forward slash "/" at end!}

\item{username}{The email address for your Go.Data login.}

\item{password}{The password for your Go.Data login}

\item{outbreak_id}{The id number for the outbreak for which you want to
download clusters.}

\item{batch_size}{For large datasets, specify the number of records to
retrieve in each iteration.}
}
\value{
Returns data frame.
}
\description{
A function to retrieve the cluster data for a
specific \code{outbreak_id}. This function relies
on the \verb{/outbreaks/\{id\}/clusters} API endpoint.
Records are imported in iterative batches
and then appended together into a single data
frame.
}
\details{
This function works on all versions of Go.Data.
}
\examples{
\dontrun{
url <- "https://MyGoDataServer.com/"
username <- "myemail@email.com"
password <- "mypassword"
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"

clusters <- get_clusters(
  url = url,
  username = username,
  password = password,
  outbreak_id = outbreak_id
)
}
}

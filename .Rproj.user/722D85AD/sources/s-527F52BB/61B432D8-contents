\name{get_cases2}
\alias{get_cases2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download cases from Go.Data (version 2.38.1 or later)
}
\description{
Download a data frame of the case line list from a specific outbreak.
}
\usage{
get_cases2(url = url, username = username, password = password, outbreak_id = outbreak_id, wait = 5)
}
\arguments{
  \item{url}{
Insert the base URL for your instance of Go.Data here. Don't forget the forward slash "/" at end!
}
  \item{username}{
The email address for your Go.Data login.
}
  \item{password}{
The password for your Go.Data login
}
  \item{outbreak_id}{
The id number for the outbreak for which you want to download cases.
}
  \item{wait}{
  The number of seconds to wait in between iterations of checking the status of the download. Default is 5 seconds, but the user can specify a smaller value to speed up the process if the dataset is small.
}
}
\details{
This function downloads the case line list from Go.Data. Behind the scenes, it is a multi-step process of 1) submitting a request to download the data, 2) asking the server if the download is ready, and 3) proceeding with the download when it is ready.

The specific API calls used are: \cr
1) GET /outbreaks/{id}/cases/export \cr
2) GET /export-logs/{id} \cr
3) GET /export-logs/{id}/download
}
\value{
Returns data frame of cases. Some fields, such as addresses, hospitalization history, and questionnaire fields will require further unnesting. See the tidyr::unnest() function.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
url <- "https://MyGoDataServer.com/"
username <- "myemail@email.com"
password <- "mypassword"
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"

cases <- get_cases2(url=url, username=username, password=password, outbreak_id=outbreak_id)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

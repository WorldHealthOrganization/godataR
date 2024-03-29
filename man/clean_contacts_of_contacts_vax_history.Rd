% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_contacts_of_contacts_vax_history.R
\name{clean_contacts_of_contacts_vax_history}
\alias{clean_contacts_of_contacts_vax_history}
\title{Cleans vaccination data from contacts of contacts data}
\usage{
clean_contacts_of_contacts_vax_history(contacts_of_contacts, language_tokens)
}
\arguments{
\item{contacts_of_contacts}{A \code{tibble} with address information from contacts
of contacts data.}

\item{language_tokens}{A tibble of language tokens returned by
\code{\link[=get_language_tokens]{get_language_tokens()}} to translate the string tokens in the data.}
}
\value{
A \code{tibble} with cleaned and un-nested vaccination history data.
}
\description{
Cleans and un-nests vaccination history, where vaccination is
complete, from contacts of contacts data. Contacts of contacts data is
returned from \code{\link[=get_contacts_of_contacts]{get_contacts_of_contacts()}}.
}
\examples{
\dontrun{
url <- "https://MyGoDataServer.com/"
username <- "myemail@email.com"
password <- "mypassword"
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"

contacts_of_contacts <- get_contacts_of_contacts(
  url = url,
  username = username,
  password = password,
  outbreak_id = outbreak_id
)

language_tokens <- get_language_tokens(
  url = url,
  username = username,
  password = password,
  language = "english_us"
)

vax_history <- clean_contacts_of_contacts_vax_history(
  contacts_of_contacts = contacts_of_contacts,
  language_tokens = language_tokens
)
}
}

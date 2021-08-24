###################################################################################################

url <- "https://godata-r13.who.int/"                   # <--------------------- insert instance url here, don't forget the slash at end !
username <- "godata_api@who.int"                           # <--------------------- insert your username for signing into Go.Data webapp here
password <- "godata_api@who"                           # <--------------------- insert your password for signing into Go.Data webapp here
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"   # <--------------- insert your outbreak ID here! (find it in URL when you have selected outbreak)

###################################################################################################

# SCRIPT TO PULL IN COLLECTIONS ACROSS ANY GO.DATA INSTANCE #
# updated 04 August 2021

###################################################################################################
# read in from Go.Data API, using your updated log-in credentials by Clicking "Source"
# no need to modify the below unless you would like to bring in additional API endpoints used in the dashboards in webapp, etc!
# Script authored and maintained by Go.Data team (godata@who.int)
###################################################################################################

# this script currently returns: 
#                                     cases, 
#                                     contacts,
#                                     contacts of contacts,
#                                     events
#                                     follow ups, 
#                                     lab results,
#                                     locations,
#                                     relationships,
#                                     teams,
#                                     users

###################################################################################################
# source required scripts, including packages that need to be installed
#       this includes set_core_fields.R script, which ensures that collections have all the columns they need and set to NA those that don't exist
#       otherwise, the JSON drops it if these questions were consistently not answered, which can break the scripts if its a core variable
###################################################################################################
if (!require("here")) install.packages("here")
source(here::here("scripts", "aaa_load_packages.R"))
#source(here::here("scripts", "set_core_fields.R"))

###################################################################################################
# FUNCTION TO GET ACCESS TOKEN
###################################################################################################

#This funciton will be used before each API call to get an access token
get_access_token <- function() {
  response <- POST(url=paste0(url,"api/oauth/token?access_token=123"),  
                   body = list(username = username,password = password),                                       
                   encode = "json") %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE)
  return(response$access_token)
}
print(get_access_token()) #Test to make sure url, username, and password are valid


###################################################################################################
# GET CASES
###################################################################################################

#get total number of cases
cases_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases/count"), 
               add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()

#Import Cases in batches 
cases <- tibble()
batch_size <- 50000 # number of records to import per iteration
skip <-0
while (skip < cases_n) {
  message("********************************")
  message(paste0("Importing records ", as.character(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
  cases.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases",
                      "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
               add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as='text') %>%
    fromJSON( flatten=TRUE) %>%
    as_tibble()
  message(paste0("Imported ", format(nrow(cases.i), scientific = FALSE)," records"))
  cases <- cases %>% bind_rows(cases.i)
  skip <- skip + batch_size
  message(paste0("Data Frame now has ", format(nrow(cases), scientific = FALSE), " records"))
  rm(cases.i)
}
rm(batch_size, skip, cases_n)


###################################################################################################
# GET CONTACTS
###################################################################################################

#get total number of contacts
contacts_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts/filtered-count"), 
                  add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()

#import contacts in batches
contacts <- tibble()
batch_size <- 50000 # number of records to import per iteration
skip <- 0
while (skip < contacts_n) {
  message("********************************")
  message(paste0("Importing records ", format(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
  contacts.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts",
                        "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
                 add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as='text') %>%
    fromJSON( flatten=TRUE) %>%
    as_tibble()
  contacts <- contacts %>% bind_rows(contacts.i)
  skip <- skip + batch_size
  message(paste0("Imported ", format(nrow(contacts.i), scientific = FALSE)," records"))
  message(paste0("Data Frame now has ", format(nrow(contacts), scientific = FALSE), " records"))
  rm(contacts.i)
}
rm(batch_size, skip, contacts_n)


###################################################################################################
# GET FOLLOWUPS
###################################################################################################

#get total number of follow-ups
followups_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/follow-ups/filtered-count"), 
                  add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()

#import follow-ups in batches
followups <- tibble()
batch_size <- 50000
skip <- 0
while (skip < followups_n) { #for (i in 1:ceiling(followups_n / batch_size)) {
  message("********************************")
  message(paste0("Importing records ", format(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
  followups.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/follow-ups",
                            "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
                     add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as='text') %>%
    fromJSON( flatten=TRUE) %>%
    as_tibble()
  message(paste0("Imported ", format(nrow(followups.i), scientific = FALSE)," records"))
  followups <- followups %>% bind_rows(followups.i)
  skip <- skip + batch_size
  message(paste0("Data Frame now has ", format(nrow(followups), scientific = FALSE), " records"))
  rm(followups.i)
}
rm(batch_size, skip, followups_n)


###################################################################################################
# GET EVENTS
###################################################################################################

#get total number of contacts
events_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/events/filtered-count"), 
                  add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()

events <- tibble()
batch_size <- 50000
skip <- 0
while (skip < events_n) {
  message("********************************")
  message(paste0("Importing records ", format(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
  events.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/events",
                           "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
                    add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as='text') %>%
    fromJSON( flatten=TRUE) %>%
    as_tibble()
  events <- events %>% bind_rows(events.i)
  skip <- skip + batch_size
  message(paste0("Imported ", format(nrow(events.i), scientific = FALSE)," records"))
  message(paste0("Data Frame now has ", format(nrow(events), scientific = FALSE), " records"))
  rm(events.i)
}
rm(batch_size, skip, events_n)


###################################################################################################
# GET CONTACTS OF CONTACTS
###################################################################################################

#get total number of contacts-of-contacts
contacts_of_contacts_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts-of-contacts/filtered-count"), 
                  add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()

#import contacts_of_contacts in batches
contacts_of_contacts <- tibble()
batch_size <- 50000
skip <- 0
while (skip < contacts_of_contacts_n) {
  message("********************************")
  message(paste0("Importing records ", format(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
  contacts_of_contacts.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts-of-contacts",
                           "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
                    add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as='text') %>%
    fromJSON( flatten=TRUE) %>%
    as_tibble()
  contacts_of_contacts <- contacts_of_contacts %>% bind_rows(contacts_of_contacts.i)
  skip <- skip + batch_size
  message(paste0("Imported ", format(nrow(contacts_of_contacts.i), scientific = FALSE)," records"))
  message(paste0("Data Frame now has ", format(nrow(contacts_of_contacts), scientific = FALSE), " records"))
  rm(contacts_of_contacts.i)
}
rm(batch_size, skip, contacts_of_contacts_n)


###################################################################################################
# GET LAB RESULTS
###################################################################################################

#get total number of lab results
lab_results_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/lab-results/filtered-count"), 
                              add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()

#import contacts_of_contacts in batches
lab_results <- tibble()
batch_size <- 50000
skip <- 0
while (skip < lab_results_n) {
  message("********************************")
  message(paste0("Importing records ", format(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
  lab_results.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/lab-results/aggregate",
                                       "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
                                add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as='text') %>%
    fromJSON( flatten=TRUE) %>%
    as_tibble()
  lab_results <- lab_results %>% bind_rows(lab_results.i)
  skip <- skip + batch_size
  message(paste0("Imported ", format(nrow(lab_results.i), scientific = FALSE)," records"))
  message(paste0("Data Frame now has ", format(nrow(lab_results), scientific = FALSE), " records"))
  rm(lab_results.i)
}
rm(batch_size, skip, lab_results_n)


###################################################################################################
# GET RELATIONSHIPS
###################################################################################################

#get total number of relationships
relationships_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/relationships/count"), 
                     add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()

#import reslationshps in batches
relationships <- tibble()
batch_size <- 50000
skip <- 0
while (skip < relationships_n) {
  message("********************************")
  message(paste0("Importing records ", format(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
  relationships.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/relationships/export",
                              "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
                       add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as='text') %>%
    fromJSON( flatten=TRUE) %>%
    as_tibble()
  relationships <- relationships %>% bind_rows(relationships.i)
  skip <- skip + batch_size
  message(paste0("Imported ", format(nrow(relationships.i), scientific = FALSE)," records"))
  message(paste0("Data Frame now has ", format(nrow(relationships), scientific = FALSE), " records"))
  rm(relationships.i)
}
rm(batch_size, skip, relationships_n)


###################################################################################################
# GET LOCATIONS
###################################################################################################

# import location hierarchy (outbreak agnostic)
locations <- GET(paste0(url,"api/locations"), 
                          add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>%
  fromJSON(flatten=TRUE) %>%
  as_tibble()


###################################################################################################
# GET TEAMS
###################################################################################################

# import Teams (outbreak agnostic)
teams <- GET(paste0(url,"api/teams"), 
                      add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>%
  fromJSON(flatten=TRUE) %>%
  as_tibble()


###################################################################################################
# GET USERS
###################################################################################################

# import Users (outbreak agnostic)
users <- GET(paste0(url,"api/users"), 
                      add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
  content(as="text") %>%
  fromJSON(flatten=TRUE) %>%
  as_tibble()


######################################################################################################
# Remove extra items from the environment, leaving only the data frames from Go.Data
######################################################################################################
rm(username, password, url, outbreak_id, get_access_token)


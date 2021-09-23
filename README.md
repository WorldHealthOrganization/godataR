# godataR: easier interfacing with with [Go.Data](https://www.who.int/tools/godata) <img src="assets/hex-godataR_nourl.png" align="right" height="139"/>

There are a range of functions within the `godataR` package to interact with your Go.Data instance's API. You must have valid Go.Data user credentials with appropriate roles/permissions to obtain access to the API and securely access data. (see *Set Parameters* section). The self-documenting description of the API methods can be viewed using Loopback Explorer by adding `/explorer` to the end of any Go.Data URL.  You can find more information on the Go.Data API [here](https://worldhealthorganization.github.io/godata/api-docs/).

## Installation
This package is hosted on the WHO Github Repository here: [https://github.com/WorldHealthOrganization/godataR](https://github.com/WorldHealthOrganization/godataR).
Install the package within your R console by executing the code below.

```
#Install package
devtools::install_github("WorldHealthOrganization/godataR")
```

## Providing parameters from your instance of Go.Data
These parameters will be used within each function, and for ease of use you can set them once after installation, alternatively you can change them at each point a function is called (for instance, to modify `outbreak_id`.

```
#Set parameters
url <- "https://MyGoDataServer.com/" #your Go.Data URL
username <- "myemail@email.com" #your email address to login
password <- "mypassword" #your password to login
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b" #the outbreak id number
```

## Execute functions to retrieve a collections
### Get a data frame of collections with godataR function, pertaining to a specific outbreak
```
cases <- godataR::get_cases2(url=url, username=username, password=password, outbreak_id=outbreak_id)

contacts <- godataR::get_contacts2(url=url, username=username, password=password, outbreak_id=outbreak_id)

contacts_of_contacts <- godataR::get_contacts_of_contacts2(url=url, username=username, password=password, outbreak_id=outbreak_id)

lab_results <- godataR::get_labresults2(url=url, username=username, password=password, outbreak_id=outbreak_id)

relationships <- godataR::get_relationships2(url=url, username=username, password=password, outbreak_id=outbreak_id)

followups <- godataR::get_followups2(url=url, username=username, password=password, outbreak_id=outbreak_id)

events <- godataR::get_events2(url=url, username=username, password=password, outbreak_id=outbreak_id)

clusters <- godataR::get_clusters(url=url, username=username, password=password, outbreak_id=outbreak_id)

```
### Get a data frame of collections with godataR function, outbreak-agnostic and related to admin settings applied to your Go.Data instance 
```
users <- godataR::get_users(url=url, username=username, password=password) 

teams <- godataR::get_teams(url=url, username=username, password=password)

locations <- godataR::get_locations(url=url, username=username, password=password)

reference_data <- godataR::get_reference_data(url=url, username=username, password=password)
```

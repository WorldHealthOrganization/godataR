# _godataR_: easier wrangling with the Go.Data API <img src="assets/hex-godataR_nourl.png" align="right" height="260"/>

## Overview
[Go.Data](https://www.who.int/tools/godata) is a software for outbreak response and contact tracing developed by WHO in collaboration with partners in the Global Outbreak Alert and Response Network (GOARN). Go.Data focusses on case and contact data including laboratory data, hospitalization and other variables collected through investigation forms. It generates contact follow-up lists and visualisation of chains of transmission.

The `godataR` package was built to allow Go.Data [user community](https://community-godata.who.int/) to more easily interact with their instance's API, especially when dealing with large volumes of data. You can now quickly retrieve a database colleciton in an un-nested format for further cleaning and analysis with one line of code. 

- For example, `get_cases()` or `get_locations()` provides a painless way to retrieve case data or hierarchical locations by bypassing all of the normal API syntax and authentication required.

Future iterations will focus on POSTing to the Go.Data API for bulk creation or modification of case, contact and lab records.

## Installation
This package is hosted on the WHO Github Repository here: [https://github.com/WorldHealthOrganization/godataR](https://github.com/WorldHealthOrganization/godataR).
Install the package within your R console by executing the code below.

```
#Install package
devtools::install_github("WorldHealthOrganization/godataR")
```
## Usage

### Provide parameters (your Go.Data credentials)
You must have valid Go.Data user credentials with appropriate roles/permissions to successfully receive an access token to make any API calls. 
You can set your parameters at the outset of your R session, to call them more easily when fetching your collections. You can also specify ad-hoc if you are working across various outbreaks.

```
#Set parameters
url <- "https://MyGoDataServer.com/" #your Go.Data URL
username <- "myemail@email.com" #your email address to login
password <- "mypassword" #your password to login
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b" #the outbreak id number
```

## Execute functions to retrieve desired collections

### The below collections require access to and specification of Outbreak.
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

### The below collections are outbreak-agnostic and applied at system-level.
```
users <- godataR::get_users(url=url, username=username, password=password) 

teams <- godataR::get_teams(url=url, username=username, password=password)

locations <- godataR::get_locations(url=url, username=username, password=password)

reference_data <- godataR::get_reference_data(url=url, username=username, password=password)
```

## Handling versioning across Go.Data releases
There were significant changes to most API endpoints at the release of V38.1, in order to increase performance during export and in-app visualization.

This applies across cases, contacts, contacts of contacts, lab results, relationships, followups, events and clusters. 

For these functions, we have 2 versions labelled 1 and 2 accordingly (i.e. `get_cases()` vs. `get_cases2()`.

We recommend upgrading to the latest Go.Data version and utilizing ***version 2*** if possible.
The function `check_godata_version()` spits out errors if you are utilizing the wrong version of the function.


## API documentation
Go.Data is running on [LoopBack](https://loopback.io/doc/index.html). You can access the self-documenting description of all available API methods in using Loopback Explorer by adding `/explorer` to the end of any Go.Data URL.  

You can find more information on the Go.Data API [here](https://worldhealthorganization.github.io/godata/api-docs/).

## How to provide feedback our contribute
Bug reports and feature requests should be posted on github under [_issues_](https://github.com/WorldHealthOrganization/godataR/issues). All other questions and feedback, feel free to email us at godata@who.int or post a query in the [Go.Data Community of Practice](https://community-godata.who.int/) 

Contributions are welcome via pull requests.

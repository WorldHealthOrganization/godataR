# godataR
An R package to interface with Go.Data software (https://www.who.int/tools/godata). 


## Example to download case data

```

#Install package
devtools::install_github("WorldHealthOrganization/godataR")

#Provide parameters from your instance of Go.Data
url <- "https://MyGoDataServer.com/" #your Go.Data URL
username <- "myemail@email.com" #your email address to login
password <- "mypassword" #your password to login
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b" #the outbreak id number

#Get a data frame of cases
cases <- godataR::get_cases2(url=url, username=username, password=password, outbreak_id=outbreak_id)
```

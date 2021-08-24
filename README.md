# godataR
Use R to access Go.Data datasets


#Example to download case data

```
#Provide parameters from your instance of Go.Data
url <- "https://MyGoDataServer.com/" #your Go.Data URL
username <- "myemail@email.com" #your email address to login
password <- "mypassword" #your password to login
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b" #the outbreak id number

#Get a data frame of cases
cases <- get_cases2(url=url, username=username, password=password, outbreak_id=outbreak_id)
```

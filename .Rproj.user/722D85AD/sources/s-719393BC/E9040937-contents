get_cases <- function(url=url, username=username, password=password, outbreak_id=outbreak_id, batch_size=50000) {

  #Check version of Go.Data
  gd.version <- get_godata_version(url=url) %>%
    as.data.frame()
  names(gd.version) <- "version"
  gd.version <- gd.version %>%
    dplyr::select(version) %>%
    tidyr::separate(version,c("a","b","c")) %>%
    dplyr::mutate_all(as.numeric)

  if (gd.version$a >= 2 & gd.version$b >= 38 & gd.version$c >= 1) {
    warning("Your version of Go.Data is 2.38.1 or later. Consider using the function get_cases2().")
  }


  #get total number of cases
  cases_n <- httr::GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases/count"),
                 add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " "))) %>%
    httr::content(as="text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    unlist() %>%
    unname()

  #Import Cases in batches
  cases <- tibble::tibble()
  batch_size <- batch_size # number of records to import per iteration
  skip <-0
  while (skip < cases_n) {
    message("********************************")
    message(paste0("Downloading records ", as.character(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
    cases.i <- httr::GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases",
                          "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"),
                   add_headers(Authorization = paste("Bearer", get_access_token(url=url, username=username, password=password), sep = " "))) %>%
      httr::content(as='text') %>%
      jsonlite::fromJSON( flatten=TRUE) %>%
      tibble::as_tibble()
    message(paste0("Downloaded ", format(nrow(cases.i), scientific = FALSE)," records"))
    cases <- cases %>%
      dplyr::bind_rows(cases.i)
    skip <- skip + batch_size
    message(paste0("Data Frame now has ", format(nrow(cases), scientific = FALSE), " records"))
    rm(cases.i)
  }
  rm(batch_size, skip, cases_n)

  df <- cases
  return(df)
}

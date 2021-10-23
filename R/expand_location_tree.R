#' Expand the tree for location data from Go.Data
#'
#' A function to add location id numbers at all
#' administrative levels.
#'
#' @param locations A data frame of locations. Can be retrieved using get_locations().
#'
#' @return
#' Returns a data frame of locations associated with Go.Data instance including all location id numbers at different administrative levels.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://MyGoDataServer.com/"
#' username <- "myemail@email.com"
#' password <- "mypassword"
#'
#' locations <- get_locations(url=url,
#'                            username=username,
#'                            password=password)
#'
#' locations_clean <- expand_location_tree(locations)
#'
#' #All in one step
#' locations_clean <- expand_location_tree(get_locations(url,username,password))
#' }
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @export


expand_location_tree <- function(locations=locations) {

  locations_clean <- locations %>%
    select(-identifiers, -synonyms) %>%
    filter(deleted == FALSE | is.na(deleted)) %>%
    filter(active == TRUE | is.na(active)) %>%
    mutate(admin_level = sub(".*LEVEL_", "", geographicalLevelId)) %>%
    select(location_id = id,
           admin_level,
           name,
           parent_location_id = parentLocationId,
           population_density = populationDensity,
           lat = geoLocation.lat,
           long = geoLocation.lng
    ) %>%
    filter(!is.na(admin_level))

  max.admin.level <- max(as.numeric(locations_clean$admin_level))
  for (i in 0:max.admin.level) {
    admin_i <- locations_clean %>% filter(admin_level==i)
    names(admin_i) <- paste0("admin_",i,"_",names(admin_i))
    admin_i$location_id <- pull(admin_i, paste0("admin_",i,"_location_id"))
    admin_i$parent_location_id <- pull(admin_i, paste0("admin_",i,"_parent_location_id"))
    assign(paste0("admin_",i), admin_i)
  }
  admin_0$parent_location_id <- NULL
  for (i in max.admin.level:1) {

    print(paste0("*****Starting Admin ", i, "*****"))

    admin_i <- get(paste0("admin_",i))

    for (x in 1:i) {
      print(paste0("*Joining Admin ", i-x, "*"))
      admin_ix <- get(paste0("admin_",i-x))
      admin_i <- left_join(admin_i, admin_ix, by=c("parent_location_id" = "location_id"))
      admin_i$parent_location_id <- admin_i$parent_location_id.y
      admin_i$parent_location_id.y <- NULL
      assign(paste0("admin_",i), admin_i)
    }
    admin_i$parent_location_id <- NULL
    assign(paste0("admin_",i), admin_i)
  }

  full <- admin_0
  for (i in 1:max.admin.level) {
    admin_i <- get(paste0("admin_",i))
    full <- full %>% bind_rows(admin_i)
  }
  locations_clean <- locations_clean %>% left_join(full, by="location_id")

  return(locations_clean)

}

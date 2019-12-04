#' Convert latitude and longitude to corresponding UTM projection
#' 
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' 
#' @export
#' 
get_local_crs <- function(lat,lon){
  # Determine UTM zone
  zone <- floor((lon+180)/6)%%60
  
  # Construct proj4 string
  local_crs <- str_c("+proj=utm +zone=",zone) %>% 
    {if_else(lat < 0,str_c(.," +south"),.)} %>% 
    str_c(" +datum=WGS84 +units=m +no_defs")
  
  return(local_crs)
}
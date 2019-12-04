#' Convert kg per ha to bushels per acre
#' 
#' @export
#' 
kg_ha_to_bu_acre <- function(kg_ha,crop='Wheat'){
  
  kg_per_ha_to_lbs_per_acre <- 1/1.12
  
  bu_acre <- kg_ha*kg_per_ha_to_lbs_per_acre/lbs_per_bushel(crop)
  
  return(bu_acre)
}

#' @export

lbs_per_bushel <- function(crop){
  lbs <- switch(crop,
                Wheat = 60,
                Soybean = 60,
                Corn = 56,
                Sorghum = 56)
  return(lbs)
}

#' @export
#' 
calculate_pixel_area <- function(raster_layer){

  # Calculate pixel area in hectares (assumes raster_layer resolution is in meters)
  pixel_area <- res(raster_layer) %>% 
    {(.[1]*.[2])/10000}
  
  return(pixel_area)
}

#' Reads shapefile of N Rich Strip and buffers for Farmer practice
#' 
#' @param shapefile_path a length-one character vector that gives the file path
#'   for the shapefile for the N rich strip
#'   
#' @export
#' 
read_n_rich_strip_farmer_practice <- function(shapefile_path,active_ndvi){

  # Read in shapefile for N rich strip
  n_rich <- st_read(shapefile_path)
  
  # Calculate area of N rich strip
  nr_area <- st_area(n_rich)
  
  area_threshold <- nr_area*.05 # Assume 5% accuracy is good enough
  
  buffer_radius <- sqrt(nr_area)/2
  
  for(i in 1:10){

    # Buffer around N rich strip to get area for farmer practice
    farmer_practice <- n_rich %>% 
      st_buffer(buffer_radius) %>% 
      st_difference(n_rich) %>% 
      dplyr::select(-dummy.1)

    fp_area <- st_area(farmer_practice)

    # Check if farmer practice area is within 5% of N rich strip area
    if(abs(fp_area - nr_area) < area_threshold){
      break
    }else{
      buffer_radius <- buffer_radius * sqrt(nr_area)/sqrt(fp_area)
    }
  }
  
  avg_nr_ndvi <- raster::extract(active_ndvi,n_rich,mean) %>% 
    as.vector()
  avg_fp_ndvi <- raster::extract(active_ndvi,farmer_practice,mean) %>% 
    as.vector()

  return(list(n_rich_strip=avg_nr_ndvi,farmer_practice=avg_fp_ndvi))
}
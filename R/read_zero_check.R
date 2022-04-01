#' Reads shapefile of Zero Check Strip and buffers for Farmer practice
#' 
#' @param shapefile_path a length-one character vector that gives the file path
#'   for the shapefile for the N rich strip
#'   
#' @param active_ndvi a raster layer that contains actively mapped NDVI
#' 
#' @param shapefile_crs a coordinate reference system description as generated
#'   by `crs()`
#'
#' @param percentile_threshold percentile at which to calculate the summary
#'   value for the N Rich Strip (percent)
#'
#' @return a list object with elements zero_n_check and farmer_practice
#'   
#' @export
#' 
read_zero_n_check_farmer_practice <- function(shapefile_path,active_ndvi,shapefile_crs=NULL,percentile_threshold=10){

  # Read shapefile using N rich strip function
  zero_check <- read_n_rich_strip_farmer_practice(shapefile_path,active_ndvi,shapefile_crs,percentile_threshold)
  
  # Return output using zero N check naming
  return(list(zero_n_check = zero_check$n_rich_strip,
              farmer_practice = zero_check$farmer_practice))
}

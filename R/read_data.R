#' Converts csv with crs latitude, longitude to spatial points data frame with crs Universal Transverse Mercator 14 (utm14).
#' 
#' This function reads in a csv file with crs latitude and longitude, converts it to a spatial points data frame, and reprojects it to crs utm zone 14.
#' @param filename The csv file with Normalized difference vegetation index (ndvi) data.
#' @return The transformed points.
#' @export
#' @examples
#' read_pts('ndvi.csv')

read_pts <- function(filename){
  require(dplyr)
  require(readr)
  require(sp)

  orig_crs  <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  utm14 <- "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  pts <- read_csv(filename) %>%
      {SpatialPointsDataFrame(select(.,long,lat),
                              select(.,ndvi),
                              proj4string = CRS(orig_crs))} %>%
      spTransform(CRSobj=CRS(utm14))

  return(pts)
}


#' Calculates ndvi
#' 
#' This function calculates ndvi as a function of near infrared) NIR and red channels.
#' @param NIR The spectral reflectance in NIR region.
#' @param Red The spectral reflectance in red or visible region.
#' @return ndvi
#' @export
#' @examples
#' calc_ndvi(0.5, 0.08)

calc_ndvi <- function(NIR,Red){
  vi <- (NIR-Red)/(NIR+Red)
  return(vi)
}


#' Converts multiband geotiff to ndvi image.
#' 
#' This function reads in a multiband geotiff file, calculates ndvi using calc_ndvi function, projects the raster into crs utm14, and returns ndvi image as a single layer.
#' @param filename The tiff file with multiple bands.
#' @return ndvi image
#' @export
#' @examples
#' read_image('MULTIBAND.tiff')

read_image <- function(filename,
                       layer_names=c('Blue', 'Green', 'Red', 'NIR',
                  'Green2', 'Red2', 'Thermal', 'Alpha')){

  utm14 <- "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  img <- raster::brick(filename)

  names(img) <- layer_names

  NDVI_img <- calc_ndvi(img[['NIR']],img[['Red2']]) %>%
      raster::projectRaster(crs=utm14)

  return(NDVI_img)
}

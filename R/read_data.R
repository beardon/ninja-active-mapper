#' Converts csv with crs latitude, longitude to spatial points data frame with crs Universal Transverse Mercator 14 (utm14).
#' 
#' This function reads in a csv file with crs latitude and longitude, converts it to a spatial points data frame, and reprojects it to crs utm zone 14.
#' @param filename The csv file with Normalized difference vegetation index (ndvi) data.
#' @return The transformed points.
#' @export
#' @examples
#' read_pts('ndvi.csv')

read_pts <- function(filename,
                     t_srs = CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")){

  orig_crs  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  pts <- read_csv(filename) %>%
      {SpatialPointsDataFrame(dplyr::select(.,long,lat),
                              dplyr::select(.,ndvi),
                              proj4string = orig_crs)} %>%
      spTransform(CRSobj=t_srs)

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


#' Calculates NDVI using equation from Sentera
#' 
#' This function calculates NDVI as a function of NIR and red channels using
#' the equation provided by Sentera
#' @param NIR The spectral reflectance in the NIR region.
#' @param red The spectral reflectance in red or visible region.
#' @return NDVI
#' @export
#' @examples
#' calc_sentera_ndvi(0.5, 0.08)

calc_sentera_ndvi <- function(NIR,red){
  vi <- ((1.236 * NIR) - (0.188 * red))/((1.000 * NIR) + (0.044 * red))
  return(vi)
}

#' Converts multiband geotiff to ndvi image.
#' 
#' This function reads in a multiband geotiff file, calculates ndvi
#'  using calc_ndvi function and returns ndvi image as a single layer.
#' @param filename The tiff file with multiple bands.
#' @return ndvi image
#' @export
#' @examples
#' read_image('MULTIBAND.tiff')

read_image <- function(filename,
                       red_band = 6,
                       nir_band = 4){

    NIR <- raster::raster(filename,band=nir_band)
    Red2 <- raster::raster(filename,band=red_band)
    
    NDVI_img <- calc_ndvi(NIR,Red2)

  return(NDVI_img)
}


#' Converts a False-color geotiff to NDVI image using Sentera's formula
#' 
#' This function reads in a false-color geotiff file, calculates NDVI
#'  using calc_sentera_ndvi() function and returns ndvi image as a single layer.
#' @param filename The tiff file with multiple bands.
#' @return ndvi image
#' @export
#' @examples
#' read_sentera('FALSE_COLOR.tiff')

read_sentera <- function(filename,
                       red_band = 1,
                       NIR_band = 3){
  
  NIR <- raster::raster(filename,band=NIR_band)
  red <- raster::raster(filename,band=red_band)
  
  NDVI_img <- calc_sentera_ndvi(NIR,red)
  
  return(NDVI_img)
}

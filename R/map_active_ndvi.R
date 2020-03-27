#' Interpolates active ndvi using regression kriging.
#'
#' This function kriges active ndvi from actively measured ndvi points and passive ndvi values from image and returns the kriged values as an image.
#' The autokrige function from automap package is used to do the kriging.
#' @param pts spdf of actively measured ndvi.
#' @param img raster layer of passive ndvi.
#' @return image of the kriged values.
#' @export

map_active_ndvi <- function(pts,img){

    # Extract passive NDVI values from img for each point in the pts dataset
    pt_data <- raster::extract(img,pts,
            method="simple",
            sp=TRUE)
    names(pt_data) <-c("active","passive")
    
    # Determine number of actively measured points that are missing NDVI values within imagery
    n_missing <- pt_data$passive %>% 
      {is.na(.)} %>% 
      sum()

    if(n_missing > 0){
      n_missing %>% 
        sprintf('From map_active_ndvi():\n %i sample points dropped due to missing values within imagery.',.) %>% 
        warning()
      
      pt_data <- pt_data@data$passive %>% 
        {!is.na(.)} %>% 
        {pt_data[.,]}
    }

    # Convert img to a SpatialPointsDataFrame for kriging with autoKrige()
    pt_out <- as(img,"SpatialPointsDataFrame")
    names(pt_out) <- "passive"
    
    # Addition: 
    
    ## Getting residuals for kriging
    fit <- betareg(active~passive, data=pt_data, link = "logit")
    active_out <- predict(fit,newdata=pt_out)
    ## Adding residuals to spdf column
    #pt_data$resid <- residuals(fit)
    ## Fitting a variogram on residuals
    #kriged_resid <- automap::autoKrige(resid~1, input_data=pt_data,new_data = pt_out)
    #back_transformed <- inv.logit(logit(active_out) + kriged_resid$krige_output$var1.pred)

    # Create copy of original image for filling in kriged values
    ndvi_active <- img

    ndvi_active[!is.na(ndvi_active)] <- active_out

    return(ndvi_active)
}

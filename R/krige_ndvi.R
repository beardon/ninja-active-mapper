#' Interpolates active ndvi using regression kriging.
#'
#' This function kriges active ndvi from actively measured ndvi points and passive ndvi values from image and returns the kriged values as an image.
#' The autokrige function from automap package is used to do the kriging.
#' @param pts spdf of actively measured ndvi.
#' @param img raster layer of passive ndvi.
#' @return image of the kriged values.
#' @export

krige_ndvi <- function(pts,img){

    # Extract passive NDVI values from img for each point in the pts dataset
    pt_data <- raster::extract(img,pts,
            method="simple",
            sp=TRUE)
    names(pt_data) <-c("active","passive")

    # Convert img to a SpatialPointsDataFrame for kriging with autoKrige()
    pt_out <- as(img,"SpatialPointsDataFrame")
    names(pt_out) <- "passive"
    
    # Addition: 
    
    ## Getting residuals for kriging
    fit <- betareg(active~passive, data=pt_data, link = "logit")
    active_out <- predict(fit,newdata=pt_out)
    ## Adding residuals to spdf column
    pt_data$resid <- residuals(fit)
    ## Fitting a variogram on residuals
    kriged_resid <- automap::autoKrige(resid~1, input_data=pt_data,new_data = pt_out)
    back_transformed <- inv.logit(logit(active_out) + kriged_resid$krige_output$var1.pred)

    # Create copy of original image for filling in kriged values
    ndvi_active <- img

    ndvi_active[!is.na(ndvi_active)] <- back_transformed

    return(ndvi_active)
}

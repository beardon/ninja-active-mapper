#' Calculate yield potential
#' 
#' @export
#' 
zero_check_fertilized_yield_potential <- function(yp_field,yp_farmer_practice,
                                                  zero_check_strip,farmer_practice,
                                                  slope,intercept,yp_cap_multiplier=1.33){
  
  check_range('zero_check_strip',zero_check_strip,ne_val=0)
  
  check_range('farmer_practice',farmer_practice,
              ne_val=0,min_val=0,max_val=1)
  
  # Because this is zero check, we divide farmer_practice by zero n check:
  response_index <- slope*(farmer_practice/zero_check_strip)+intercept
  
  yp_fertilized <- yp_field*response_index
  
  # Cap fertilized yield potential at 20% above yield potential of farmer practice times the response index
  yp_max <- yp_farmer_practice*response_index*yp_cap_multiplier

  yp_fertilized[yp_fertilized > yp_max] <- yp_max
  
  return(yp_fertilized)
}

#' Calculate yield potential based on NDVI
#' 
#' @export
#' 
zero_check_yield_potential <- function(NDVI, GDD, A, b, YPmax = Inf){
  
  YP <- yield_potential(NDVI,GDD,A,b)
  
  YP[YP > YPmax] <- YPmax
  
  return(YP)
}
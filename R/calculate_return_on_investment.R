
#' Function for calculating the return on investment for variable rate nitrogen application
#'
#' @param yield_uniform_N yield prediction with uniform rate nitrogen application
#' @param yield_no_N yield prediction without any nitrogen application
#' 
#' @return The SBNRC adjusted Nitrogen rate
#' @export
#' @examples
#' 
calculate_return_on_investment <- function(yield_fertilized,yield_field,
                                           N_rate,
                                           grain_price,N_price){

  income <- (yield_fertilized - yield_field) * grain_price

  cost <- N_rate * N_price
  
  roi <- income - cost
  
  return(roi)
  
}

#' @export
#' 
calculate_return_on_vrn <- function(yield_vrn,yield_flat,
                                    N_excess,
                                    grain_price,N_price){
  
  income <- (yield_vrn - yield_flat) * grain_price
  
  savings <- N_excess * N_price
  
  roi <- income + savings
  
  return(roi)
  
}

#' @export
#' 
calculate_yield_flat_rate <- function(yield_fertilized,yield_field,
                                 N_flat,N_crop){

  yield_flat_rate <- yield_field + N_flat/N_crop

  yield_flat_rate[yield_flat_rate > yield_fertilized] <-
    yield_fertilized[yield_flat_rate > yield_fertilized]

  yield_loss <- yield_fertilized - yield_flat_rate
  
  return(yield_loss)
}

#' @export
#' 
calculate_N_excess <- function(N_rec,N_flat){
  
  N_excess <- N_flat - N_rec
  N_excess[N_excess < 0] <- 0
  
  return(N_excess)

}

#' @export
#' 
return_on_investment_vrn <- function(yield_fertilized,yield_field,
                                 N_rec,N_flat,N_crop,
                                 grain_price,N_price){
  
  # Calculate pixel area in hectares
  pixel_area <- res(yield_fertilized) %>% 
  {(.[1]*.[2])/10000}
  
  income <- sum(yield_fertilized - yield_field) * grain_price * pixel_area
  
  cost <- sum(N_rec) * N_price * pixel_area
  
  roi <- income - cost
  
  return(roi)
  
}

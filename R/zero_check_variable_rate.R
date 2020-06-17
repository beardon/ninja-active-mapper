#' Creates a variable rate nitrogen application image based on Nitrogen Rich Strip (n_rich_strip), Farmer/Target (farmer_practice), Growing Degree Days (GDD), and active_ndvi(Target).
#'
#' @param yld_fert_field Unfertilized yield prediction for field (kg/ha)
#' @param yld_ptnl_field Fertilized yield prediction for field (kg/ha)
#' @param yld_ptnl_zero_check Fertilized yield prediction for field (kg/ha)
#' @param Napp fertilizer applied prior to sensing (kg/ha)
#' @param grain_N expected grain Nitrogen concentration (fraction)
#' @param NUE Nitrogen use efficiency (fraction)
#' 
#' @return Variable nitrogen rate recommendation using the zero check strip algorithm
#' @export
#' 

zero_check_variable_rate <- function(yld_fert_field,yld_ptnl_field,yld_ptnl_zero_check,
                                  Napp,grain_N=0.024,NUE=0.60){
  
  variable_rate_N = ((yld_fert_field - yld_ptnl_field) + (yld_fert_field - yld_ptnl_zero_check))* grain_N / NUE - Napp*NUE
  
  variable_rate_N[ variable_rate_N < 0 ] <- 0
  
  return(variable_rate_N)
}